%%---------------------------------------------------
%% $Id: update.erl 5346 2013-11-27 11:05:28Z rolong $
%%
%% 自动执行命令
%%
%% 命令：u:start()  %% 成功启动后会生成文件夹update和update/history
%%       u:stop()
%%
%% 说明：定时检查文件(update/command.txt)
%%       如果存在则执行并重新命名到update/history
%%       执行成功则生成 update/ok.txt
%%       执行失败则生成 update/error.txt
%%       每次执行前会先删除老的 ok.txt
%%       command.txt文件内容为erlang命令，
%%       可以多行，以,或.结尾(最后一行必段以.结尾)
%% @author rolong@vip.qq.com
%%----------------------------------------------------

-module(update).
-behaviour(gen_fsm).  
-export(
    [
        init/1
        ,handle_event/3
        ,handle_sync_event/4 
        ,handle_info/3
        ,terminate/3
        ,code_change/4
    ]
).
-export(
    [
        start/0
        ,start_link/1
        ,stop/0
        ,watting/2
        ,check/0
        ,check/1
        ,update/0
        ,update/1
        ,update/2
    ]
).
-include_lib("kernel/include/file.hrl").
-include("common.hrl").

check() -> check(5).
check(command_file) ->
    case file:open("update/command.txt",read) of
        {ok, _S} -> ok;
        _ -> file_not_founded
    end;
check(S) when is_integer(S) ->
    case file:list_dir(get_path()) of
        {ok, FileList} -> 
            Files = get_new_file(FileList, S * 60),
            info("---------check modules---------~n~w~n"
                "=========check modules=========", [Files]);
        Any -> info("Error Dir: ~w", [Any])
    end;
check(_) -> info("ERROR===> Badarg", []).

update() -> update(5).

update(A) ->
    update(A, "").

update(m, Prefix) ->
    StartTime = util:unixtime(),
    info("----------makes----------", []),
    make:all(),
    EndTime = util:unixtime(),
    Time = EndTime - StartTime,
    info("Make Time : ~w s", [Time]),
    update(Time / 60, Prefix);

update(S, Prefix) when is_number(S) ->
    Path = get_path(),
    case file:list_dir(Path) of
        {ok, FileList} -> 
            T = util:ceil(S * 60) + 3,
            info("Time:~w s", [T]),
            Files = get_new_file(FileList, T),
            AllZone = [#node{id = 1, name = 'myserver1@127.0.0.1'}],
            info("---------modules---------~n~w~n----------nodes----------", [Files]),
            loads(AllZone, Files, Prefix),
            util:term_to_string(Files);
        Any -> info("Error Dir: ~w", [Any])
    end;
update(Files, Prefix) when is_list(Files) ->
    AllZone = [#node{id = 1, name = 'myserver1@127.0.0.1'}],
    info("---------modules---------~n~w~n"
        "----------nodes----------", [Files]),
    loads(AllZone, Files, Prefix);
update(_, _) -> info("ERROR===> Badarg", []).

%% 更新到所有线路
loads([], _Files, _) -> 
    info("----------- ok ----------"),
    ok;
loads([_H | T], Files, Prefix) ->
    %% info("#~w -> ~s", [H#node.id, H#node.name]),
    %% rpc:cast(H#node.name, update, load, [Files]),
    load(Files, Prefix),
    loads(T, Files, Prefix).

get_new_file(Files, S) -> 
    get_new_file(get_path(), Files, S, []).
get_new_file(_Path, [], _S, Result) -> Result;
get_new_file(Path, [H | T], S, Result) ->
    NewResult = case string:tokens(H, ".") of
        [Left, Right] when Right =:= "beam" ->
            case file:read_file_info(Path ++ H) of
                {ok, FileInfo} -> 
                    Now = calendar:local_time(),
                    case calendar:time_difference(FileInfo#file_info.mtime, Now) of
                        {Days, Times} -> 
                            Seconds = calendar:time_to_seconds(Times), 
                            case Days =:= 0 andalso Seconds < S of
                                true ->
                                    FileName = list_to_atom(Left),
                                    [FileName | Result];
                                false -> Result
                            end;
                        _ -> Result
                    end;
                _ -> Result
            end;
        _ -> Result
    end,
    get_new_file(Path, T, S, NewResult).

load([], _Prefix) -> ok;
load([FileName1 | T], Prefix) ->
    FileName3 = case is_atom(FileName1) of
        true -> atom_to_list(FileName1);
        false -> FileName1
    end,
    case Prefix =/= "" andalso string:str(FileName3, Prefix) =/= 1 of
        true -> ok;
        false ->
            FileName = case is_list(FileName1) of
                true -> list_to_atom(FileName1);
                false -> FileName1
            end,
            {{Y, M, D}, {H, I, S}} = erlang:localtime(),
            TimeString = io_lib:format("[~w-~w-~w ~w:~w:~w]", [Y, M, D, H, I, S]),
            case code:soft_purge(FileName) of
                true -> 
                    case code:load_file(FileName) of
                        {module, _} -> 
                            info("loaded: ~s", [FileName]);
                        {error, What} -> 
                            case filelib:is_dir("update") of
                                false -> file:make_dir("update");
                                true -> skip
                            end,
                            LoadErrorInfo = io_lib:format("~s ERROR===> loading: ~w (~w)\n", [TimeString, FileName, What]),
                            info("~s", [LoadErrorInfo]),
                            file:write_file("update/error.txt", LoadErrorInfo, [append])
                    end;
                false -> 
                    case filelib:is_dir("update") of
                        false -> file:make_dir("update");
                        true -> skip
                    end,
                    PurgeErrorInfo = io_lib:format("~s ERROR===> Processes lingering : ~w \n", [TimeString, FileName]),
                    info("~s", [PurgeErrorInfo]),
                    file:write_file("update/error.txt", PurgeErrorInfo, [append]),
                    ok
            end
    end,
    load(T, Prefix).

%%----------------------------------------------------
%% 自动执行命令
%% 
%% @author rolong@vip.qq.com
%%----------------------------------------------------

start() -> 
    start_link(15).

start_link(Time) ->  
    ?INFO("start(~w)...", [Time]),
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, Time, []).

stop() ->
    gen_fsm:send_all_state_event(u, stop).
  
init(Time) ->  
    %% 文件夹不存在则创建
    case filelib:is_dir("update") of
        false -> file:make_dir("update");
        true -> skip
    end,
    {ok, watting, {Time}, Time * 1000}.  
  
handle_event(stop, _StateName, State) ->
    info("stopped!"),
    {stop, normal, State}.

handle_sync_event(_Any, _From, StateName, State) ->
    {reply, {error, unhandled}, StateName, State}.

handle_info(_Any, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Any, _StateName, _Opts) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

watting(timeout, {Time}) ->
    update_file(),
    {next_state, watting, {Time}, Time * 1000}.

update_file() ->
    FileDir = "./update/",
    case file:list_dir(FileDir) of
        {ok, FileList} -> 
            case get_beam_file(FileDir, FileList, []) of
                [] -> ok;
                Files -> 
                    update_file(Files),
                    load(Files, "")
            end;
        Any -> info("Error Dir: ~w", [Any])
    end.

update_file([H | T]) ->
    backup_file(H),
    move_file(H),
    update_file(T);
update_file([]) -> ok.

get_path() ->
    [Path] = case filelib:is_dir("./ebin") of
        true -> 
            %% 开发版ebin目录
            ["./ebin"];
        false ->
            %% 发布版ebin目录
            Paths = code:get_path(),
            F = fun(P) ->
                    string:str(P, "/myserver-") > 0
            end,
            lists:filter(F, Paths)
    end,
    Path ++ "/".

get_beam_file(_Path, [], Result) -> Result;
get_beam_file(Path, [H | T], Result) ->
    NewResult = case string:tokens(H, ".") of
        [Left, "beam"] ->
            [Left | Result];
        [V, "tar", "gz"] ->
            %% Process upgrade package
            process_upgrade(V, H),
            Result;
        _ -> Result
    end,
    get_beam_file(Path, T, NewResult).

process_upgrade(V, File) ->
    case file:rename("./update/" ++ File, "./releases/" ++ File) of
        ok -> 
            upgrade(V),
            ok;
        {error, RenameErr} -> 
            info("Rename Error when process_upgrade:~s", [RenameErr]),
            ok
    end.

move_file(Mod) -> 
    Path = get_path(),
    case file:rename("./update/"++Mod++".beam", Path++Mod++".beam") of
        ok -> ok;
        {error, RenameErr} -> 
            info("Rename Error when move_file:~s", [RenameErr]),
            ok
    end.

backup_file(Mod) ->
    {{Y, M, D}, {H, I, S}} = erlang:localtime(),
    TimeString = io_lib:format("~w-~w-~w_~w-~w-~w", [Y, M, D, H, I, S]),
    Path = get_path(),
    FileName = Path++Mod++".beam",
    case filelib:is_file(FileName) of
        true ->
            case file:rename(Path++Mod++".beam", Path++Mod++".beam."++TimeString++".bak") of
                ok -> ok;
                {error, RenameErr} -> 
                    info("Rename Error when backup_file:~s", [RenameErr]),
                    ok
            end;
        false -> skip
    end.

info(V) ->
    info(V, []).
info(V, P) ->
    io:format(V ++ "~n", P).

%% install release
upgrade(ReleasePackage) ->
    {ok, Vsn} = release_handler:unpack_release(ReleasePackage),
    ?INFO("Unpacked Release ~p", [Vsn]),
    {ok, OtherVsn, Desc} = release_handler:check_install_release(Vsn),
    {ok, OtherVsn, Desc} = release_handler:install_release(Vsn), 
    ?INFO("Installed Release ~p", [Vsn]),
    ok = release_handler:make_permanent(Vsn),
    ?INFO("Made Release ~p Permanent", [Vsn]).
