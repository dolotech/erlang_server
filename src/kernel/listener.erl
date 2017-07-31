%%----------------------------------------------------
%% $Id: listener.erl 3966 2013-10-26 06:32:02Z rolong $
%%
%% Tcp listener
%%----------------------------------------------------
-module(listener).
-behaviour(gen_server).
-export([start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").

start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

stop() ->
    ?INFO("close ~w...", [?MODULE]),
    supervisor:terminate_child(myserver_sup, acceptor_sup),
    supervisor:terminate_child(myserver_sup, listener),
    ok.

init([Port]) ->
    ?INFO("start ~w...", [?MODULE]),
    process_flag(trap_exit, true),
    {ok, TcpOptions} = application:get_env(tcp_options),
    {ok, TcpAccpetorNum} = application:get_env(tcp_acceptor_num),
    case gen_tcp:listen(Port, TcpOptions) of
        {ok, LSock} ->
            start_acceptor(TcpAccpetorNum, LSock),
            {ok, state};
        {error, Reason}->
            ?ERR("Can not listen ~w:~w", [Port, Reason]),
            {stop, listen_failure, state}
    end.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

start_acceptor(0, _LSock)-> ok;
start_acceptor(N, LSock)->
    case supervisor:start_child(acceptor_sup, [LSock]) of
        {ok, _Pid} -> ok;
        Else -> ?ERR("~p", [Else])
    end,
    start_acceptor(N - 1, LSock).
