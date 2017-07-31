%%----------------------------------------------------
%% 角色模块
%% $Id: role.erl 5794 2013-12-09 02:36:54Z rolong $
%% @author Rolong<rolong@vip.qq.com>
%%----------------------------------------------------

-module(role).
-behaviour(gen_server).
-export([
        create/1
        ,create/3
        ,fix_conn/6
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("offline.hrl").

-define(T_SLOW_CALL, 300).

%% === 对外接口 ===

%% 创建一个角色进程
-spec create(Socket) -> {ok, Pid} | {error, Error} | ignore when
    Socket :: port(),
    Pid :: pid(),
    Error :: term().

create(Socket) ->
    gen_server:start(?MODULE, [Socket], []).

%% 创建一个离线状态的角色地程
-spec create(offline, Key, Val) ->
    {ok, Pid} | {error, Error} | ignore when
    Key :: account_id | role_id | name,
    Val :: integer() | binary(),
    Pid :: pid(),
    Error :: term().

create(offline, Key, Val) ->
    gen_server:start(?MODULE, [offline, Key, Val], []).

%% 修复进程链接(处理掉线重连或重复登录的问题)
fix_conn(RolePid, ConnPid, PidSerder, Socket, Ip1, Port) ->
    %% 先通知接管者暂停接收数据
    connection:switch_role_pid(ConnPid, RolePid),
    Ip = util:ip_to_binary(Ip1),
    gen_server:call(RolePid, {fix_conn, ConnPid, PidSerder, Socket, Ip, Port}).

%% === 服务器内部实现 ===

init([Socket]) ->
    %% ?INFO("+~w", [self()]),
    process_flag(trap_exit, true),
    %% self() ! gc,
    case inet:peername(Socket) of
        {ok, {Ip1, Port}} ->
            Ip = util:ip_to_binary(Ip1),
            Rs = #role{pid = self(), socket = Socket, ip = Ip, port = Port},
            {ok, Rs};
        {error, Reason} ->
            R = inet:format_error(Reason),
            {stop, R}
    end;

init([offline, Key, Val]) ->
    ?INFO("create offline role! [~w:~w] ~w", [Key, Val, self()]),
    %% io:format("&"),
    process_flag(trap_exit, true),
    Pid = self(),
    Rs = #role{pid = self()},
    case lib_role:db_init(Key, Val, Rs) of
        {ok, Rs1} ->
            %% save to offline table
            ets:insert(offline, #offline{
                    id              = Rs1#role.id
                    ,account_id     = Rs1#role.account_id
                    ,pid            = Pid
                    ,name           = Rs1#role.name
                }
            ),
            set_stop_timer(?OFFLINE_CACHE_TIME),
            {ok, Rs1};
        {error, Reason} ->
            ?WARN("Error When Init Role(offline): ~w", [Reason]),
            {stop, normal}
    end.

%%' 每隔300秒执行一次hibernate
%% handle_info(gc, Rs) ->
%%     erlang:send_after(300000, self(), gc),
%%     proc_lib:hibernate(gen_server, enter_loop, [?MODULE, [], Rs]);
%%.

%%' 转发Socket数据
handle_info({socket_data, Bin}, Rs) ->
    Rs#role.pid_sender ! {data, Bin},
    {noreply, Rs};
%%.

%%' 初始化与客户端的连接进程
handle_info({event, start_client}, Rs) ->
    {ok, PidConn} = connection:start_link(self(), Rs#role.socket),
    case gen_tcp:controlling_process(Rs#role.socket, PidConn) of
        ok ->
            PidConn ! loop,
            ok;
        Reason ->
            ?ERR("error when controlling_process:~w, "
                "self():~w, PidConn:~w", [Reason, self(), PidConn]),
            self() ! disconnect
    end,
    {noreply, Rs#role{pid_conn = PidConn}};
%%.

%%' 还末登陆Socket就断开了
handle_info(disconnect, #role{status = 0} = Rs) ->
    ?INFO("Stop when no login!"),
    {stop, normal, Rs};
%%.

%%' 断开Socket连接
handle_info(disconnect, Rs) ->
    disconnect(Rs),
    %% ?INFO("~s(~w) disconnect!", [Rs#role.name, Rs#role.id]),
    %% ?INFO("~w disconnect!", [Rs#role.id]),
    ?LOG({logout, Rs#role.id, 1}),
    set_stop_timer(?OFFLINE_CACHE_TIME),
    {noreply, Rs};
%%.

%%' 结束进程
handle_info(stop, Rs) ->
    case Rs#role.status of
        0 -> {stop, normal, Rs};
        _ ->
            case logout(Rs) of
                {true, Rs1} -> {stop, normal, Rs1};
                {false, Rs1} ->
                    %% 约5~10分钟后继续尝试退出
                    Time = util:rand(5 * 60000, 10 * 60000),
                    set_stop_timer(Time),
                    {noreply, Rs1}
            end
    end;
%%.

%%' 收到链接进程发来的退出信号
handle_info({'EXIT', Pid, _Why}, Rs) ->
    if
        Pid == Rs#role.pid_conn ->
            %% Socket连接进程退出
            %% 客户端断开了连接
            self() ! disconnect;
        Pid == Rs#role.pid_sender ->
            %% 发包进程退出，发包有异常
            ?WARN("stop pid_sender: ~w, slef():~w", [Pid, self()]),
            self() ! disconnect;
        true ->
            %% 收到了其它进程的退出信号
            ?WARN("stop pid: ~w, slef():~w", [Pid, self()]),
            ok
    end,
    {noreply, Rs};
%%.

%%' 处理handle事件，仅限于内部调用
%%  Cmd为四位的协议号
handle_info({pt, Cmd, Data}, Rs) ->
    case routing(Cmd, Data, Rs) of
        {noreply, Rs1} when is_record(Rs1, role) ->
            {noreply, Rs1};
        {noreply, Rs1} ->
            ?WARNING("Not record role: ~w", [Rs1]),
            {noreply, Rs};
        {error, handle} ->
            {noreply, Rs};
        {Reply, _Rs1} ->
            ?WARNING("Ignore reply: ~w", [Reply]),
            {noreply, Rs}
    end;
%%.

%%' 服务器关闭处理
handle_info(shutdown, Rs) ->
    is_port(Rs#role.socket) andalso sender:send_error(Rs#role.socket, 1001),
    PidConn = Rs#role.pid_conn,
    case is_pid(PidConn) andalso is_process_alive(PidConn) of
        true -> Rs#role.pid_conn ! shutdown;
        false -> ok
    end,
    erlang:send_after(800, self(), {shutdown, ?MODULE}),
    {noreply, Rs};

handle_info({shutdown, _From}, Rs) ->
    PidConn = Rs#role.pid_conn,
    case is_pid(PidConn) andalso is_process_alive(PidConn) of
        true -> erlang:exit(PidConn, kill);
        false -> ok
    end,
    %% 如果是已登陆的角色，进行logout处理
    case Rs#role.status > 0 of
        true -> self() ! stop;
        false -> ok
    end,
    ?LOG({logout, Rs#role.id, 2}),
    %% io:format("-"),
    {stop, normal, Rs};
%%.

%%' 设置离线角色的内存驻留时间
handle_info({set_stop_timer, Time}, Rs) ->
    set_stop_timer(Time),
    {noreply, Rs};
%%.

%%' 设置发包进程
handle_info({set_pid_sender, PidSender}, Rs) ->
    %% 在系统繁忙时发包器到这里时有可能已经挂掉，所以需要判断下
    case is_pid(PidSender) andalso is_process_alive(PidSender) of
        false ->
            ?LOG({logout, Rs#role.id, 4}),
            ?WARNING("*** WARNING *** when set_pid_sender", []),
            self() ! stop,
            {noreply, Rs};
        true ->
            {noreply, Rs#role{pid_sender = PidSender}}
    end;
%%.

%%' test
handle_info(test, Rs) ->
    A = lists:foldl( fun(P, Acc0) -> [{P, erlang:process_info(P, registered_name), erlang:process_info(P, memory), erlang:process_info(P, message_queue_len), erlang:process_info(P, current_function), erlang:process_info(P, initial_call)} | Acc0] end, [], [self()]),
    io:format("sender:~n~p", [A]),
    erlang:send_after(5000, self(), test),
    {noreply, Rs};
%%.

handle_info(_Info, Rs) ->
    ?WARNING("Not matched info: ~w", [_Info]),
    {noreply, Rs}.

%%' 修复连接进程
handle_call({fix_conn, ConnPid, PidSerder, Socket, Ip, Port}, _From, Rs) ->
    %% 如果有退出定时器则清除它
    erase_stop_timer(),
    case is_pid(Rs#role.pid_conn) andalso is_process_alive(Rs#role.pid_conn) of
        false -> ok;
        true ->
            %% 通知原角色已在别处登陆
            sender:send_error(Rs#role.socket, 1002),
            %% 如果当前角色正在被别的客户端控制，则将其断开
            erlang:unlink(Rs#role.pid_conn),
            erlang:exit(Rs#role.pid_conn, kill),
            ok
    end,
    %% 开始修复连接进程
    erlang:link(ConnPid),
    Status1 = case Rs#role.status of
        0 -> 1;
        S -> S
    end,
    NewRs = Rs#role{
        pid_conn = ConnPid
        ,pid_sender = PidSerder
        ,socket = Socket
        ,ip = Ip
        ,port = Port
        ,status = Status1
    },
    state_save(NewRs),
    connection:resume_after(ConnPid, 0), %% 稍后通知接管者继续接收数据
    {reply, NewRs, NewRs};
%%.

%%' 处理和客户端交互的Socket事件
handle_call({pt, Cmd, Data}, _From, Rs) ->
    case routing_check(Cmd, Rs) of
        true ->
            case routing(Cmd, Data, Rs) of
                {Reply, Rs1} when is_record(Rs1, role) ->
                    {reply, Reply, Rs1};
                {error, handle} ->
                    {reply, noreply, Rs};
                {Reply, Rs1} ->
                    ?WARN("Not record role: ~w", [Rs1]),
                    {reply, Reply, Rs}
            end;
        false ->
            ?INFO("routing_check error[Cmd: ~w Data:~w]", [Cmd, Data]),
            erlang:exit(Rs#role.pid_conn, kill),
            {reply, noreply, Rs}
    end;
%%.

%% 取得当前角色的完整属性
handle_call(get_state, _From, Rs) ->
    {reply, {ok, Rs}, Rs};

%%' 踢人下线
handle_call({force_logout, _Reason}, _From, Rs) ->
    ?INFO("force_logout:~s(~w)", [Rs#role.name, Rs#role.id]),
    is_port(Rs#role.socket) andalso gen_tcp:close(Rs#role.socket),
    is_pid(Rs#role.pid_conn) andalso erlang:exit(Rs#role.pid_conn, kill),
    self() ! stop,
    %% 发包进程会自动退出吗？
    ?LOG({logout, Rs#role.id, 3}),
    {reply, ok, Rs};
%%.

handle_call(_Request, _From, Rs) ->
    {noreply, Rs}.

%%' 处理和客户端交互的Socket事件
handle_cast({pt, Cmd, Data}, Rs) ->
    T = erlang:now(),
    case routing_check(Cmd, Rs) of
        true ->
            case routing(Cmd, Data, Rs) of
                {noreply, Rs1} when is_record(Rs1, role) ->
                    test(T, Cmd),
                    {noreply, Rs1};
                {Reply, Rs1} when is_record(Rs1, role) ->
                    case pt_pack:p(Cmd, Reply) of
                        {ok, Bin} ->
                            Rs#role.pid_sender ! {data, Bin},
                            test(T, Cmd);
                        {error, Error} ->
                            ?WARN("~w:~w, data:~w", [Error, Cmd, Reply]),
                            ok
                    end,
                    {noreply, Rs1};
                {_Reply, Rs1} ->
                    ?WARNING("Not record role: ~w", [Rs1]),
                    {noreply, Rs1}
            end;
        false ->
            ?INFO("routing_check failure! [Cmd: ~w Data:~w]", [Cmd, Data]),
            erlang:exit(Rs#role.pid_conn, kill),

            {noreply, Rs}
    end;
%%.

handle_cast(Msg, Rs) ->
    ?WARN("Undefined msg:~w", [Msg]),
    {noreply, Rs}.

%%' 进程结束时的处理
terminate(_Reason, Rs) ->
    case Rs#role.id > 0 of
        true ->
            ets:delete(online, Rs#role.id),
            ets:delete(offline, Rs#role.account_id),
            ok;
        false ->
            ok
    end,
    %% ?INFO("terminate acc:~s, name ~s, id:~w, reason:~w", [Rs#role.account_id, Rs#role.name, Rs#role.id, _Reason]),
    %% eprof:stop_profiling(),
    %% eprof:analyze(),
    ok.
%%.

code_change(_OldVsn, Rs, _Extra) ->
    {ok, Rs}.

%% --- 私有函数 ------------------------------

%%' 更新在线表中的角色数据
state_save(Rs) ->
    case Rs#role.id > 0 of
        false ->
            %% 没有登录角色的不写入ETS表中
            ignore;
        true ->
            RoleOnline = case ets:lookup(online, Rs#role.id) of
                [RO] -> RO;
                _ -> #online{}
            end,
            ets:insert(online, RoleOnline#online{
                    id              = Rs#role.id
                    ,account_id     = Rs#role.account_id
                    ,pid            = Rs#role.pid
                    ,pid_sender     = Rs#role.pid_sender
                    ,name           = Rs#role.name
                    ,status         = Rs#role.status
                    ,lev            = Rs#role.lev
                    ,sex            = Rs#role.sex
                    ,growth         = Rs#role.growth
                }
            )
    end.
%%.

%%' 回存数据到数据库
%%  save_to_db(Rs, Save) -> NewRs.
save_to_db(Rs, []) -> 
    {true, Rs#role{save = []}};

save_to_db(Rs, [items | S]) ->
    T = erlang:now(),
    #role{id = Rid, items = Items} = Rs,
    {Status, Items1} = mod_item:db_update(Rid, Items),
    Rs1 = Rs#role{items = Items1},
    case Status of
        true ->
            DT = timer:now_diff(erlang:now(), T) / 1000,
            ?DEBUG("save items: ~w ms", [DT]),
            save_to_db(Rs1, S);
        false -> 
            SaveDelay = util:store_element(items, Rs#role.save_delay),
            {false, Rs1#role{
                save = S
                ,save_delay = SaveDelay
            }}
    end;

%% 保存英雄数据
save_to_db(Rs, [heroes | S]) ->
    T = erlang:now(),
    #role{id = Rid, heroes = Heroes} = Rs,
    {Status, Heroes1} = mod_hero:db_update(Rid, Heroes),
    Rs1 = Rs#role{heroes = Heroes1},
    case Status of
        true ->
            DT = timer:now_diff(erlang:now(), T) / 1000,
            ?DEBUG("save heroes: ~w ms", [DT]),
            save_to_db(Rs1, S);
        false -> 
            SaveDelay = util:store_element(heroes, Rs#role.save_delay),
            {false, Rs1#role{
                save = S
                ,save_delay = SaveDelay
            }}
    end;

%% 保存角色数据
save_to_db(Rs, [role | S]) ->
    T = erlang:now(),
    case lib_role:db_update(Rs) of
        {ok, _} ->
            DT = timer:now_diff(erlang:now(), T) / 1000,
            ?DEBUG("save role: ~w ms", [DT]),
            save_to_db(Rs, S);
        {error, _} -> 
            SaveDelay = util:store_element(role, Rs#role.save_delay),
            {false, Rs#role{
                save = S
                ,save_delay = SaveDelay
            }}
    end;

%% 保存酒馆数据
save_to_db(Rs, [tavern | S]) ->
    T = erlang:now(),
    case mod_hero:db_update_tavern(Rs) of
        {ok, _} ->
            DT = timer:now_diff(erlang:now(), T) / 1000,
            ?DEBUG("save role: ~w ms", [DT]),
            save_to_db(Rs, S);
        {error, _} -> 
            %% 保存出错，
            %% 不再继续执行保存操作，
            %% 将tavern标识转存到save_delay中，
            %% 进程退出时再尝试保存
            SaveDelay = util:store_element(tavern, Rs#role.save_delay),
            {false, Rs#role{
                save = S
                ,save_delay = SaveDelay
            }}
    end;
save_to_db(Rs, [Type | S]) ->
    ?WARN("undefined save type: ~w", [Type]),
    save_to_db(Rs, S).
%%.

%%' 连接断开处理，把在线表中的数据转移到离线表
%%  disconnect(Rs) -> ok.
disconnect(Rs) ->
    #role{
        id = Rid
        ,account_id = AccountId
        ,pid = Pid
        ,name = Name
        ,growth = Growth
    } = Rs,
    %% save to offline table
    ets:insert(offline, #offline{
            id              = Rid
            ,account_id     = AccountId
            ,pid            = Pid
            ,name           = Name
            ,growth = Growth
        }
    ),
    %% delete online data
    ets:delete(online, Rs#role.id),
    ok.
%%.

%%' 断开连接后，正真退出游戏，回存数据。
%%  logout(Rs) -> {true, NewRs} | {false, NewRs}.
logout(Rs) ->
    AddSaves = [role, heroes, items],
    Save = util:del_repeat_element(AddSaves ++ Rs#role.save),
	mod_attain:attain_stores(Rs),
	mod_arena:arena_stores(Rs),
    case save_to_db(Rs, Save) of
        {true, Rs1} ->
            save_to_db(Rs1, Rs1#role.save_delay);
        {false, Rs1} ->
            {false, Rs1}
    end.
%%.

%%' test
test(T, Cmd) ->
    DT = timer:now_diff(erlang:now(), T) / 1000,
    case DT > ?T_SLOW_CALL of
        false -> ok;
        true ->
            ?INFO("slow handle [Cmd:~w, T:~w]", [Cmd, DT]),
            ok
    end.
%%. test

%%' 设置离线角色的内存驻留时间
set_stop_timer(Time) ->
    erase_stop_timer(),
    Ref = erlang:send_after(Time, self(), stop),
    put('$stop_timer', Ref).

erase_stop_timer() ->
    case get('$stop_timer') of
        undefined -> ok;
        TimerRef ->
            erlang:cancel_timer(TimerRef),
            erase('$stop_timer')
    end.

%% get_stop_timer() ->
%%     get('$stop_timer').

%%.

%%' 协议路由前检查，如果末登陆，只允许部分协议交互
routing_check(Cmd, Rs) ->
    case lists:member(Cmd, [11000, 11001, 11002, 11003, 11101, 11102, 11103]) of
        true -> true;
        false -> Rs#role.id > 0
    end.
%%.

%%' 协议路由
routing(Cmd, Data, Rs) ->
    Module = pt_module_map:get(Cmd),
    try
        case Module:handle(Cmd, Data, Rs) of
            {ok} -> {noreply, Rs};
            {ok, Reply} when is_list(Reply) -> 
                ?DEBUG("Cmd:~w, Reply:~w", [Cmd, Reply]),
                {Reply, Rs};
            {ok, NewRs} ->
                state_save(NewRs),
                {_, Rs0} = save_to_db(NewRs, NewRs#role.save),
                {noreply, Rs0};
            {ok, Reply, NewRs} ->
                state_save(NewRs),
                ?DEBUG("Cmd:~w, Reply:~w", [Cmd, Reply]),
                {_, Rs0} = save_to_db(NewRs, NewRs#role.save),
                {Reply, Rs0};
            {error, Reason} ->
                erlang:exit(Rs#role.pid_conn, kill),
                ?WARN("Cmd:~w, Data:~w, Reason:~w", [Cmd, Data, Reason]),
                {noreply, Rs};
            Else ->
                erlang:exit(Rs#role.pid_conn, kill),
                ?WARN("Cmd:~w, unexpected data:~w", [Cmd, Else]),
                {noreply, Rs}
        end
    catch
        T:X ->
            Arg = [Rs#role.id, Cmd, Data, T, X, erlang:get_stacktrace()],
            ?ERR("HANDLE ERROR~n[Rid:~w, CMD=~w, DATA=~p, ~w:~w]~n~p", Arg),
            {error, handle}
    end.
%%.

%% vim: set filetype=erlang foldmarker=%%',%%. foldmethod=marker:
