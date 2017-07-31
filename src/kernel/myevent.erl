%%----------------------------------------------------
%% $Id: myevent.erl 5346 2013-11-27 11:05:28Z rolong $
%%
%% 事件处理
%%----------------------------------------------------
-module(myevent).
-behaviour(gen_server).
-export([
        start_link/0
        ,save_events/0
        ,send_event/2
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").

-define(ROLE_EVENTS_TABLE_NAME, role_events).
-define(ROLE_EVENTS_FILE_NAME, "dets/role_events").


-record(state, {}).

%%' === 对外接口 ===

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send_event(Rid, Event) ->
    gen_server:cast(?MODULE, {send_event, Rid, Event}).

save_events() ->
    ?INFO("save_events(~w)...", [?MODULE]),
    gen_server:call(?MODULE, save_events).

%% === 服务器内部实现 ===

init([]) ->
    ?INFO("start ~w...", [?MODULE]),
    init_role_events(),
    State = #state{},
    {ok, State}.

handle_call(save_events, _From, State) ->
    try
        ets:tab2file(?ROLE_EVENTS_TABLE_NAME, ?ROLE_EVENTS_FILE_NAME),
        ?INFO("Save role_events:~p", 
            [ets:info(?ROLE_EVENTS_TABLE_NAME, size)]),
        {reply, ok, State}
    catch
        T:X ->
            ?ERR("[~p:~p]", [T, X]),
            {reply, error, State}
    end;

handle_call(_Request, _From, State) ->
    ?INFO("Undefined request:~p", [_Request]),
    {noreply, State}.

%% 角色登陆
handle_cast({login, Rid, Pid}, State) ->
    %% 登陆完成后处理离线事件
    case get_events(Rid) of
        [] -> ok;
        Events -> 
            Pid ! {pt, 1002, Events},
            set_events(Rid, [])
    end,
    %% 服务器启动后第一次登陆
    %% case erlang:get({login, Rid}) of
    %%     true -> ok;
    %%     undefined ->
    %%         erlang:put({login, Rid}, true),
    %%         Pid ! {pt, 1003, []},
    %%         ok
    %% end,
    {noreply, State};

handle_cast({send_event, Rid, Event}, State) when is_integer(Rid) ->
    ?INFO("~w", [{send_event, Rid, Event}]),
    add_event(Rid, Event),
    {noreply, State};

handle_cast({send_event, Rids, Event}, State) ->
    ?INFO("~w", [{send_event, Rids, Event}]),
    do_send_event(Rids, Event),
    {noreply, State};

handle_cast(save_role_events, State) ->
    try
        ets:tab2file(?ROLE_EVENTS_TABLE_NAME, ?ROLE_EVENTS_FILE_NAME),
        {noreply, State}
    catch
        TT:X ->
            ?ERR("[~p:~p]", [TT, X]),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    ?INFO("undefined msg:~p", [_Msg]),
    {noreply, State}.

handle_info(clear_login, State) ->
    F = fun
        ({{login, Id}, _}) ->
            erlang:erase({login, Id});
        (_) -> ok
    end,
    lists:foreach(F, erlang:get()),
    {noreply, State};

handle_info(info, State) ->
    ?INFO("role_events [size:~w, memory:~w]", 
        [ets:info(?ROLE_EVENTS_TABLE_NAME, size), 
            ets:info(?ROLE_EVENTS_TABLE_NAME, memory)]),
    {noreply, State};

handle_info(_Info, State) ->
    ?INFO("undefined info:~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -- 内部函数 --

init_role_events() ->
    case filelib:is_file(?ROLE_EVENTS_FILE_NAME) of
        true ->
            ets:file2tab(?ROLE_EVENTS_FILE_NAME),
            ?INFO("Init role_events! [size:~w, memory:~w]", 
                [ets:info(?ROLE_EVENTS_TABLE_NAME, size),
                    ets:info(?ROLE_EVENTS_TABLE_NAME, memory)]),
            ok;
        false ->
            ets:new(?ROLE_EVENTS_TABLE_NAME, 
                [{keypos, 1}, named_table, protected, set]),
            ?INFO("New role_events!", []),
            ok
    end.

%% 处理新发布的事件
do_send_event([Rid | Rids], Event) ->
    add_event(Rid, Event),
    do_send_event(Rids, Event);
do_send_event([], _Event) -> ok.

add_event(Rid, Event) ->
    case lib_role:get_online_pid(role_id, Rid) of
        false -> set_event(Rid, Event);
        Pid -> Pid ! {pt, 1001, Event}
    end.

get_events(Rid) ->
    case ets:lookup(?ROLE_EVENTS_TABLE_NAME, Rid) of
        [{_, Es}] -> Es;
        [] -> [];
        Else ->
            ?WARNING("Undefined role_event: ~w", [Else]),
            []
    end.

%% 注意：默认忽略相同事件
set_event(Rid, Event) ->
    Events = get_events(Rid),
    case lists:member(Event, Events) of
        true -> ok;
        false ->
            Events1 = [Event | Events],
            ets:insert(?ROLE_EVENTS_TABLE_NAME, {Rid, Events1})
    end.

set_events(Rid, Events) ->
    ets:insert(?ROLE_EVENTS_TABLE_NAME, {Rid, Events}).

%% del_event(Rid, Event) ->
%%     Events = get_events(Rid),
%%     case lists:member(Event, Events) of
%%         true ->
%%             Events1 = lists:delete(Event, Events),
%%             ets:insert(?ROLE_EVENTS_TABLE_NAME, {Rid, Events1}),
%%             true;
%%         false -> false
%%     end.
