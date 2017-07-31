%%----------------------------------------------------
%% Robot
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(robot).
-include("common.hrl").
-include("robot.hrl").

-define(SERVER, robot).
-define(TCP_OPTS, [binary, {packet, 0}, {nodelay, true}, {delay_send, false}, {exit_on_close, false}]).
-define(TCP_HANDSHAKING,    <<"ABCDEFGHIJKLMN876543210">>).

-record(state, {
        online_roles     = [] %% [{AccId, Rid, Pid}]
    }).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
        start_link/0
        ,login/1
        ,login/2
        ,unpack/2
        ,recv_process/1
        ,bat_login/1
    ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

bat_login(Num) ->
    robot ! {login_robots, Num}.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    ?INFO("start ~w...", [?MODULE]),
    process_flag(trap_exit, true),
    State = #state{},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?WARN("Undefined msg:~w", [Msg]),
    {noreply, State}.

handle_info(count, State) ->
    ?INFO("online_roles: ~w", [length(State#state.online_roles)]),
    {noreply, State};

handle_info({login_ok, Aid, Rid, Pid}, State) ->
    State1 = case lists:keymember(Aid, 1, State#state.online_roles) of
        false ->
            OnlineRoles = [{Aid, Rid, Pid} | State#state.online_roles],
            State#state{online_roles = OnlineRoles};
        true ->
            State
    end,
    {noreply, State1};

%% 每分钟执行一次
handle_info(loop, State) ->
    {_, {_H, M, _}} = erlang:localtime(),
    case (M div 5) =:= 0 of
        true -> ok;
        false -> ok
    end,
    erlang:send_after(60000, self(), loop),
    {noreply, State};

%% TEST
handle_info({login_robots, Num}, State) ->
    Nth = case get(login_robots_nth) of
        undefined -> 
            put(login_robots_nth, 0),
            0;
        N -> 
            put(login_robots_nth, N + 1),
            N + 1
    end,
    Start = Nth * Num + 1,
    End = Start + Num - 1,
    Ids = lists:seq(Start, End),
    spawn(fun() -> login_robots(Ids, State#state.online_roles) end),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
%%' login
login(Aid) ->
    login(Aid, robot_handle).
login(Aid, HandleName) ->
    case gen_tcp:connect("127.0.0.1", 8100, ?TCP_OPTS) of
        {ok, Socket} ->
            ?DEBUG("connect(~w) ok!~n", [Aid]),
            PidSender = spawn_link(fun() -> sender(Socket, 0) end),
            %% init state
            R = #robot{aid = Aid, pid_sender = PidSender, socket = Socket,
                      handle_name = HandleName},
            Pid = spawn_link(?MODULE, recv_process, [R]),
            ok = gen_tcp:controlling_process(Socket, Pid),
            gen_tcp:send(Socket, ?TCP_HANDSHAKING), %% 发送握手消息
            %% login
            AidB = list_to_binary([?AID_PREFIX, integer_to_list(Aid)]),
            %% TimeS = integer_to_list(util:unixtime()),
            %% ServerIdS = <<"1">>,
            %% {_, ServerKey} = application:get_env(myserver, server_key),
            %% M = list_to_binary([TimeS, AidB, ServerIdS, ServerKey]),
            %% Signature = util:md5(M),
            %% LoginKey = list_to_binary([
            %%         <<"account_id=">>,AidB,<<"&serverid=1&time=">>
            %%     ,TimeS,<<"&signature=">>,Signature]),
            %% PidSender ! {cmd, 11001, [LoginKey]},
            Key = "23d7f859778d2093",
            Rand = util:rand(0, 255),
            Signature = util:md5(list_to_binary([
                        "1" 
                        ,integer_to_list(Rand)
                        ,Key
                        ,AidB
                    ])),
            PidSender ! {cmd, 11000, [2, Rand, 1, AidB, AidB, Signature]},
            {ok, Pid};
        {error, Reason} ->
            io:format("connect error: ~w~n", [Reason]),
            {error, Reason}
    end.
%%.

unpack(Rs, BinIn = <<Len:16, Cmd:16, Bin:Len/binary, Bin1/binary>>) ->
    case pt_unpack_client:p(Cmd, Bin) of
        {ok, Data} ->
            %% io:format("[Recv] ~w:~w~n", [Cmd, Data]),
            %% io:format("[Recv] ~w:~w~n", [Cmd, length(Data)]),
            HandleName = Rs#robot.handle_name,
            Rs1 = HandleName:handle(Cmd, Data, Rs),
            Rs2 = case is_record(Rs1, robot) of
                true -> Rs1;
                false -> Rs
            end,
            case get(cmd) of
                undefined -> ok;
                Cmd ->
                    erase(cmd),
                    io:format("~n[Recv] ~w:~w~n", [Cmd, Data]),
                    ok;
                _ -> 
                    io:format("~n[Recv] ~w:~w~n", [Cmd, Data]),
                    ok
            end,
            RestLen = byte_size(Bin1),
            case RestLen > 1048576 of
                true ->
                    ?INFO("RestBin too larger: ~w", [RestLen]),
                    self() ! force_stop,
                    ok;
                false ->
                    unpack(Rs2, Bin1)
            end;
        {error, _Reason} ->
            {Rs, BinIn}
    end;
unpack(Rs, BinIn) -> {Rs, BinIn}.

%%' 接收消息
recv_process(#robot{socket = Socket} = R) ->
    receive
        {tcp, Socket, BinIn} ->
            BinIn2 = case R#robot.tmp_package of
                <<>> -> BinIn;
                Tmp -> list_to_binary([Tmp, BinIn])
            end,
            {R1, RestBin} = unpack(R, BinIn2),
            R2 = R1#robot{tmp_package = RestBin},
            recv_process(R2);
        {tcp_closed, Socket} ->
            R#robot.pid_sender ! stop,
            ok;
        {tcp_error, Socket, Reason} ->
            R#robot.pid_sender ! stop,
            ?INFO("tcp_error:~w", [Reason]);
        {info, Info} ->
            ?INFO("Info:~w", [Info]);
        test ->
            ?INFO("*** TEST ***", []),
            ok;
        stop ->
            R#robot.pid_sender ! stop,
            gen_tcp:close(Socket),
            ok;
        force_stop ->
            R#robot.pid_sender ! stop,
            gen_tcp:close(Socket),
            ?INFO("force_stop! [Rid:~w]", [R#robot.aid]);
        {cmd, Cmd, Data} ->
            put(cmd, Cmd),
            io:format("~n[Send] ~w:~w~n", [Cmd, Data]),
            R#robot.pid_sender ! {cmd, Cmd, Data},
            recv_process(R);
        Other ->
            R#robot.pid_sender ! stop,
            gen_tcp:close(Socket),
            ?INFO("undefined msg: ~w", [Other])
    end.
%%.

%%' Socket数据发包器
sender(Socket, Index) ->
    receive
        {cmd, Cmd, Data} ->
            {ok, Bin} = pt_pack_client:p(Cmd, Data, Index),
            case gen_tcp:send(Socket, Bin) of
                ok -> 
                    Index1 = case Index >= 255 of
                        false -> Index + 1;
                        true -> 0
                    end,
                    sender(Socket, Index1);
                {error,closed} -> 
                    ok;
                Error -> io:format("Error when send data: ~w~n", [Error])
            end;
        stop -> ok;
        Other ->
            io:format("Error packet:~w~n", [Other]),
            sender(Socket, Index)
    end.
%%.

login_robots([AccId | T1], Online) ->
    case lists:keymember(AccId, 1, Online) of
        false -> spawn(fun() -> login(AccId) end);
        true -> ok
    end,
    %% util:sleep(1),
    login_robots(T1, Online);
login_robots(_, _) -> ok.

%%% vim: set filetype=erlang foldmarker=%%',%%. foldmethod=marker:
