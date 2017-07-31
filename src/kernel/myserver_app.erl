%% -------------------------------------------------
%% myserver application
%%
%% $Id: myserver_app.erl 5772 2013-12-07 09:34:35Z rolong $
%%
%% @author Rolong<rolong@vip.qq.com>
%% -------------------------------------------------

-module(myserver_app).
-behaviour(application).
-export([start/2, prep_stop/1, stop/1]).

-include("common.hrl").
-include("offline.hrl").

start(_Type, _Args) ->
    io:format("start myserver ...~n"),
    %% 初始化环境变量
    myenv:init(),
    %% 初始化数据库连接
    init_mysql(),
    inets:start(),
    %% 创建DETS目录
    case filelib:is_dir(?DETS_DIR) of
        false -> file:make_dir(?DETS_DIR);
        true -> ok
    end,
    %% 初始化ETS
    init_ets(),
    %% 启动监督树
    Port = env:get(tcp_port),
    case myserver_sup:start_link([Port]) of
        {ok, SupPid} -> 
            %% 初始化日志记录
            LogFileName = "log/error_runtime.log",
            CustomLogFileName = "log/error_custom.log",
            error_logger:logfile({open, LogFileName}),
            mylogger:logfile({open, CustomLogFileName}),
            %% 修复异常登陆日志
            catch db:execute("UPDATE `log_login` SET `event`= 4, `logout_time`= ~s WHERE event = 0", [util:unixtime()]),
            %% 启动定时任务
            supervisor:start_child(myserver_sup, {crontab, {crontab, start_link, []}, temporary, 10000, worker, [crontab]}),
            %% 导入竞技场机器人数据
            case db:get_one("select count(*) from arena") of
                {ok, 0} -> mod_arena:import_robot();
                _ -> ok
            end,
            %% 开启调试模式
            %% u:d(),
            {ok, SupPid};
        {error, Error} -> 
            {error,Error}
    end.

prep_stop(_State) ->
    ?INFO("prep_stop(~w)...", [?MODULE]),
    listener:stop(), %% 首先停止接收新的连接
    lib_role:stop(), %% 强制让在线玩家退出
    %% 保存事件管理器中的数据
    myevent:save_events(),
    %% 保存管理/统计进程中的数据
    gen_server:call(admin, save_data),
    %% 暂停一秒
    util:sleep(1000),
    ok.


stop(_State) ->
    ok.

init_ets() ->
    %% 在线表
    ets:new(online, [{keypos, #online.id}, named_table, public, set]),
    %% 离线表
    ets:new(offline, [{keypos, #offline.account_id}, named_table, public, set]),
    ok.

init_mysql() ->
    {ok, DbHost} = application:get_env(myserver, db_host),
    {ok, DbPort} = application:get_env(myserver, db_port),
    {ok, DbUser} = application:get_env(myserver, db_user),
    {ok, DbPass} = application:get_env(myserver, db_pass),
    {ok, DbName} = application:get_env(myserver, db_name),
    {ok, DbEncode} = application:get_env(myserver, db_encode),
    {ok, DbConnNum} = application:get_env(myserver, db_connector_num),
    LogFun = fun
        (_Mod, _Line, _, _P) -> ok
    end,
    mysql:start_link(?DB, DbHost, DbPort, DbUser, DbPass, DbName, LogFun, DbEncode), %% 与mysql数据库建立连接
    util:for(1, DbConnNum,
        fun(_I) ->
                mysql:connect(?DB, DbHost, DbPort, DbUser, DbPass, DbName, DbEncode, true)
        end
    ),
    ok.
