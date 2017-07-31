%%----------------------------------------------------
%% Client connection
%% 
%% $Id: connection.erl 5648 2013-12-05 03:55:00Z rolong $
%%
%% @author Rolong<rolong@vip.qq.com>
%%----------------------------------------------------
-module(connection).
-behaviour(gen_server).
-export([
        start_link/2
        ,switch_role_pid/2
        ,resume_after/2
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
-define(TCP_TIMEOUT, 30000).
-define(HEADER_LENGTH, 5).
-define(T_SLOW_CALL, 300).
-define(MAX_ERROR, 5).

-record(state, {
        pid_role
        ,pid_sender
        ,socket
        ,pause
        ,nth = 0
        ,process_mode = sync %% sync | async
    }
).

%% --- 对外接口 ---

%% 新建连接
start_link(PidRole, Socket) ->
    gen_server:start_link(?MODULE, [PidRole, Socket], []).

%% 切换控制的角色并暂停接收数据
switch_role_pid(PidConn, PidRole) ->
    case is_pid(PidConn) andalso is_process_alive(PidConn) of
        false -> false;
        true -> PidConn ! {switch_role_pid, PidRole} 
    end.

%% 重新开始接收数据
resume_after(PidConn, Time) when is_integer(Time) ->
    case is_pid(PidConn) andalso is_process_alive(PidConn) of
        false -> false;
        true ->
            case Time of
                0 -> PidConn ! resume;
                _ -> erlang:send_after(Time, PidConn, resume)
            end
    end.

%% --- 服务器内部实现 ---

init([PidRole, Socket]) ->
    PidConn = self(),
    PidSender = spawn_link(fun() -> sender(PidConn, Socket, 0) end),
    %% ?INFO("pid_sender:~w", [PidSender]),
    PidRole ! {set_pid_sender, PidSender},
    State = #state{pid_role = PidRole, pid_sender = PidSender, socket = Socket, pause = false},
    {ok, State}.

handle_call(_Request, _From, State) ->
    ?WARNING("unexpected request: ~w", [_Request]),
    {noreply, State}.

handle_cast(_Msg, State) ->
    ?WARNING("unexpected msg: ~w", [_Msg]),
    {noreply, State}.

%% 循环接收消息
handle_info(loop, #state{pause = Pause, socket = Socket, nth = Nth, 
        process_mode = ProcessMode, pid_role = PidRole, pid_sender = PidSender} = State) ->
    case Pause of
        %% 暂停接收
        true -> {noreply, State};
        false ->
            case gen_tcp:recv(Socket, ?HEADER_LENGTH) of
                {ok, <<BodyLen:16, Num:8, Cmd:16>>} ->
                    ?DEBUG("Cmd:~w, BodyLen:~w", [Cmd, BodyLen]),
                    case Num of
                        Nth ->
                            case BodyLen of
                                0 -> process(ProcessMode, PidRole, PidSender, Cmd, []);
                                _ -> 
                                    %% 继续读取消息体
                                    case gen_tcp:recv(Socket, BodyLen, ?TCP_TIMEOUT) of
                                        {ok, Binary} ->
                                            %% ?INFO("Binary:~w", [Binary]),
                                            case pt_unpack:p(Cmd, Binary) of
                                                {ok, Data} ->
                                                    %% 正确解包
                                                    %% ?INFO("RECV [Cmd:~w, Data:~w]", [Cmd, Data]),
                                                    process(ProcessMode, PidRole, PidSender, Cmd, Data);
                                                {error, _Reason} ->
                                                    ?ERR("recv undefined cmd:~w", [Cmd])
                                            end;
                                        {error, Reason} ->
                                            %% 可能是网络不稳定
                                            ?WARN("recv error:~w]", [Reason]),
                                            util:sleep(500)
                                    end
                            end,
                            Nth1 = case Nth >= 255 of
                                false -> Nth + 1;
                                true -> 0
                            end,
                            self() ! loop,
                            {noreply, State#state{nth = Nth1}};
                        _ -> 
                            ?ERR("Package index error [Server:~w, Client:~w, Len:~w, Cmd:~w]", 
                                [Nth, Num, BodyLen, Cmd]),
                            util:sleep(500),
                            {stop, normal, State}
                    end;
                {error, closed} -> 
                    {stop, normal, State};
                {error, _Reason} -> 
                    ?WARN("stop recv:~w", [_Reason]),
                    {stop, normal, State}
            end
    end;

handle_info(test, State) ->
    ?DEBUG("Pid:~w", [self()]),
    {noreply, State};

handle_info(shutdown, State) ->
    State#state.pid_role ! {shutdown, ?MODULE},
    {noreply, State#state{pause = true}};

%% 更换控制的角色进程并暂停接收数据
handle_info({switch_role_pid, PidRole}, State) ->
    {noreply, State#state{pid_role = PidRole, pause = true}};

%% 继续接收数据
handle_info(resume, State) ->
    self() ! loop,
    {noreply, State#state{pause = false}};

handle_info({set_process_mode, Mode}, State) ->
    {noreply, State#state{process_mode = Mode}};

handle_info(_Info, State) ->
    ?WARNING("unexpected info: ~w", [_Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    gen_tcp:close(State#state.socket),
    erlang:exit(State#state.pid_sender, kill),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --- 私有函数 ---

%% 调用角色进程
process(sync, PidRole, PidSender, Cmd, Data) ->
    T = erlang:now(),
    case catch gen_server:call(PidRole, {pt, Cmd, Data}, 5000) of
        noreply -> ok;
        {'EXIT', Reason} ->
            ?WARNING("Error when call Role! [CMD=~w, DATA=~w, Reason:~w]", [Cmd, Data, Reason]),
            ok;
        Reply -> 
            %% ?INFO("~w, Reply:~w", [Cmd, Reply]),
            case pt_pack:p(Cmd, Reply) of
                {ok, Bin} ->
                    PidSender ! {data, Bin},
                    %% Slow calling logger
                    DT = timer:now_diff(erlang:now(), T) / 1000,
                    case DT > ?T_SLOW_CALL of
                        false -> ok;
                        true ->
                            ?INFO("slow call [Cmd:~w, T:~w]", [Cmd, DT]),
                            ok
                    end;
                {error, Error} ->
                    ?WARN("~w:~w, data:~w", [Error, Cmd, Reply]),
                    ok
            end
    end;

process(async, PidRole, _PidSender, Cmd, Data) ->
    try gen_server:cast(PidRole, {pt, Cmd, Data}) catch
        T:X ->
            ?WARNING("Error when cast Role[~w:~w]:[CMD=~w,DATA=~w]:~w", 
                [T, X, Cmd, Data, erlang:get_stacktrace()])
    end.

%% Socket data sender
sender(PidConn, Socket, ErrCount) ->
    receive
        {data, Bin} ->
            case gen_tcp:send(Socket, Bin) of
                ok -> 
                    sender(PidConn, Socket, 0);
                _ ->
                    case ErrCount < ?MAX_ERROR of
                        true ->
                            sender(PidConn, Socket, ErrCount + 1);
                        false ->
                            util:sleep(500),
                            erlang:exit(PidConn, kill),
                            gen_tcp:close(Socket)
                    end
            end;
        stop -> 
            ?DEBUG("stop sender:~w", [self()]),
            ok
    end.
