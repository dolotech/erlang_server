%%----------------------------------------------------
%% 进程监控器
%%
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(myserver_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).

-include("common.hrl").

%% @hidden
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% @hidden
init([Port]) ->
    gen_event:swap_handler(alarm_handler, {alarm_handler, swap}, {myalarm_h, []}), 
    List = [
        {mylogger, {mylogger, start_link, []}, permanent, 10000, worker, [mylogger]}
        ,{myrandomseed, {myrandomseed, start_link, []}, permanent, 10000, worker, [myrandomseed]}
        ,{cache, {cache, start_link, []}, permanent, 10000, worker, [cache]}
        ,{acceptor_sup, {acceptor_sup, start_link, []}, permanent, 10000, supervisor, [acceptor_sup]}
        ,{listener, {listener, start_link, [Port]}, permanent, 10000, worker, [listener]}
        ,{admin, {mod_admin, start_link, []}, permanent, 10000, worker, [mod_admin]}
        ,{myevent, {myevent, start_link, []}, permanent, 10000, worker, [myevent]}
        ,{arena, {mod_arena, start_link, []}, permanent, 10000, worker, [mod_arena]}
        ,{luck, {mod_luck, start_link, []}, permanent, 10000, worker, [mod_luck]}
        ,{robot, {robot, start_link, []}, permanent, 10000, worker, [robot]}
    ],
    {ok, {{one_for_one, 5, 1}, List}}.
