%%---------------------------------------------------
%% Erlang模块热更新到所有线路（包括server的回调函数，如果对state有影响时慎用）
%%
%% 检查：u:c()                %% 列出前5分钟内编译过的文件
%%       u:c(N)               %% 列出前N分钟内编译过的文件
%%
%% 更新：u:u()                %% 更新前5分钟内编译过的文件               
%%       u:u(N)               %% 更新前N分钟内编译过的文件 
%%       u:u([mod_xx, ...])   %% 指定模块（不带后缀名）
%%       u:u(m)               %% 编译并加载文件
%%       u:m()                %% 编译并加载文件
%%
%% Tips: u - update, c - check
%% 
%% @author rolong@vip.qq.com
%%---------------------------------------------------

-module(u).
-export([
    make/0
    ,c/0
    ,c/1
    ,u/0
    ,u/1
    ,m/0
    ,d/0
    ,debug_on/0
    ,debug_off/0
    ,online/0
    ,info/0
    ,info/1
    ,l/0
    ,l/1
	,ph/0
	,ph/1
    ,show_rs/1 
    ,update_client_data/0
    ,commit_client_data/0
    ,update_erlang_pt/0
    ,commit_erlang_pt/0
    ,update_erlang_data/0
    ,commit_erlang_data/0
    ,update_client_pt/0
    ,commit_client_pt/0
    ,make_and_update_data/0
    ,db/0
    ,db_truncate/0
    ,print_map/1
    ]).

-include("common.hrl").

d() ->
    case env:get(debug) of
        on -> myenv:set(debug, off), debug_off;
        _ -> myenv:set(debug, on), debug_on
    end.

print_map(MapId) ->
    Data = data_map:get(MapId),
    {Pos1, Pos2} = util:get_val(pos, Data),
    L1 = io_lib:format("~32.2.0B",[Pos1]),
    L2 = io_lib:format("~32.2.0B",[Pos2]),
    L = lists:flatten(L1 ++ L2),
    lists:foldl(fun(C, Index) ->
                        io:format("~s", [[C]]),
                        case Index rem 12 of
                            0 -> io:format("~n", []);
                            _ -> ok
                        end,
                        Index + 1
                end, 1, L),
    ok.

db_truncate() ->
    db:execute("truncate table role;"),
    db:execute("truncate table item;"),
    db:execute("truncate table hero;").

db() ->
    Sql = <<"alter table role add tollgate_id smallint(5) default 1 comment '关卡ID' after lev">>,
    db:execute(Sql).

update_client_data() ->
    Rt = os:cmd("svn update /home/default/web/simple_xls --username rolong --password erlang --no-auth-cache"),
    os:cmd("chown www:www /home/default/web/simple_xls/*.txt"),
    Rt.


commit_client_data() ->
    os:cmd("svn add -q /home/default/web/simple_xls/*.txt"),
    os:cmd("svn ci -m 'auto commit' /home/default/web/simple_xls --username rolong --password erlang --no-auth-cache").

update_client_pt() ->
    Rt = os:cmd("svn update /data/yingxiong/src/game/net/data --username myserver --password 01 --no-auth-cache"),
    os:cmd("chown -R www:www /data/yingxiong/src/game/net/data"),
    Rt.

commit_client_pt() ->
    os:cmd("svn add -q /data/yingxiong/src/game/net/data/c/* /data/yingxiong/src/game/net/data/s/* /data/yingxiong/src/game/net/data/vo/*"),
    os:cmd("svn ci -m 'auto commit' /data/yingxiong/src/game/net/data --username myserver --password 01 --no-auth-cache").

update_erlang_pt() ->
    Rt = os:cmd("svn update /data/myserver/src/pt --username rolong --password erlang --no-auth-cache"),
    os:cmd("chown www:www /data/myserver/src/pt/pt_pack.txt"),
    os:cmd("chown www:www /data/myserver/src/pt/pt_unpack.txt"),
    Rt.

commit_erlang_pt() ->
    os:cmd("chown www:www /data/myserver/src/pt/pt_pack*"),
    os:cmd("chown www:www /data/myserver/src/pt/pt_unpack*"),
    os:cmd("svn ci -m 'auto commit' /data/myserver/src/pt --username rolong --password erlang --no-auth-cache").

update_erlang_data() ->
    Rt = os:cmd("svn update /data/myserver/src/data --username rolong --password erlang --no-auth-cache"),
    os:cmd("chown www:www /data/myserver/src/data/*"),
    Rt.

commit_erlang_data() ->
    os:cmd("chown www:www /data/myserver/src/data/*.erl"),
    os:cmd("svn add -q /data/myserver/src/data/*.erl"),
    os:cmd("svn ci -m 'auto commit' /data/myserver/src/data --username rolong --password erlang --no-auth-cache").

show_rs(Id) ->
    case lib_role:get_role_pid(role_id, Id) of
        false -> false;
        Pid -> Pid ! {pt, 1005, []}
    end.

l() -> robot:login(100).
l(Id) -> robot:login(Id).

ph() -> robot:login(100, robot_handle_p).
ph(Id) -> robot:login(Id, robot_handle_p).

c() -> update:check().
c(A) -> update:check(A).

u() -> update:update(5).
u(A) -> update:update(A).

m() -> 
    %% pt_tool:main(),
    update:update(m).

make_and_update_data() -> 
    StartTime = util:unixtime(),
    c:cd("./src/data"),
    io:format("--------make data--------~n", []),
    make:all(),
    c:cd("../../"),
    EndTime = util:unixtime(),
    Time = EndTime - StartTime,
    io:format("Make Time : ~w s~n", [Time]),
    update:update((Time / 60), "data_").

make() ->
    pt_tool:main(),
    os:cmd("./rebar compile").

%% 调试开关
debug_on() -> 
    gen_event:notify(mylogger, {debug, on}),
    debug_on.

debug_off() -> 
    gen_event:notify(mylogger, {debug, off}),
    debug_off.

info(process) ->
    P = self(),
    Data = {P, erlang:process_info(P, memory), erlang:process_info(P, message_queue_len)},
    io:format("~n~w", [Data]).

%% @spec info() -> ok
%% @doc 输出系统信息, 具体内容见log/info_xxxx.log文件
info() ->
    SchedId      = erlang:system_info(scheduler_id),
    SchedNum     = erlang:system_info(schedulers),
    ProcCount    = erlang:system_info(process_count),
    ProcLimit    = erlang:system_info(process_limit),
    ProcMemUsed  = erlang:memory(processes_used),
    ProcMemAlloc = erlang:memory(processes),
    MemTot       = erlang:memory(total),
    Info = io_lib:format( "abormal termination:
        ~n   Scheduler id:                         ~p
        ~n   Num scheduler:                        ~p
        ~n   Process count:                        ~p
        ~n   Process limit:                        ~p
        ~n   Memory used by erlang processes:      ~p
        ~n   Memory allocated by erlang processes: ~p
        ~n   The total amount of memory allocated: ~p
        ~n",
        [SchedId, SchedNum, ProcCount, ProcLimit,
            ProcMemUsed, ProcMemAlloc, MemTot]),
    {{Y, M, D}, {H, M2, S}} = erlang:localtime(),
    F = fun(Int) ->
        case Int < 10 of
            true -> "0" ++integer_to_list(Int);
            false -> integer_to_list(Int)
        end
    end,
    DateStr = lists:concat([[F(X) || X <- [Y, M, D]], "_", [F(X) || X <- [H, M2, S]]]),
    File1 = "log/info_" ++ DateStr ++ ".log",
    A = lists:foldl( fun(P, Acc0) -> [{P, erlang:process_info(P, registered_name), erlang:process_info(P, memory), erlang:process_info(P, message_queue_len), erlang:process_info(P, current_function), erlang:process_info(P, initial_call)} | Acc0] end, [], erlang:processes()),
    B = io_lib:format("~s~n~p", [Info, A]),
    file:write_file(File1, B),
    io:format("~s", [Info]),
    ok.

%% 查看在线信息
online() ->
    L = ets:tab2list(online),
    do_online(L, 0),
    io:format("\n\nOnline role num: ~w\n\n", [length(L)]),
    ok.

do_online([O | T], Index) when Index < 30 ->
    io:format("\n#~-5w - ~ts", [O#online.id, O#online.name]),
    do_online(T, Index + 1);
do_online(_, _) -> ok.
