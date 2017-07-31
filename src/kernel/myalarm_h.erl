%%----------------------------------------------------
%% 警报管理 
%%
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(myalarm_h).
-behaviour(gen_server).

-export([init/1, handle_event/2, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").

%% init(Args) must return {ok, State}
init(_Args) ->
    %io:format("*** cc_alarm_handler init:~p~n",[Args]),
    {ok, 0}.

handle_event({set_alarm, {overload, []}}, N) ->
    ?WARN("overload:~w", [N+1]),
    {ok, N+1};

handle_event({clear_alarm, overload}, N) ->
    ?WARN("overload:~w", [N]),
    {ok, N};

handle_event(Event, N) ->
    ?WARN("*** alarm: ~p~n ***",[Event]),
    {ok, N}.
    
handle_call(_Request, N, _State) -> Reply = N, {ok, Reply,  N}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, N)    -> {ok, N}.
terminate(_Reason, _N)   -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.
