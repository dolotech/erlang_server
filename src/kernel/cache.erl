%%----------------------------------------------------
%% 缓存数据
%%
%% $Id$
%% @author Rolong<rolong@vip.qq.com>
%%----------------------------------------------------
-module(cache).
-behaviour(gen_server).
-export([
        start_link/0
        ,get/1
        ,set/2
        ,clean/0
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").

-record(state, {}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set(Key, Val) ->
    gen_server:cast(?MODULE, {set_cache, Key, Val}).

get(Key) ->
    case ets:lookup(?MODULE, Key) of
        [{_, Val}] -> Val;
        [] -> undefined;
        Else ->
            ?WARNING("Undefined cache value: ~w", [Else]),
            undefined
    end.

clean() ->
    ets:delete_all_objects(cache).

%% === 服务器内部实现 ===

init([]) ->
    ?INFO("start ~w...", [?MODULE]),
    case ets:info(?MODULE) of
        undefined ->
            ets:new(?MODULE, [{keypos, 1}, named_table, public, set]),
            ok;
        false ->
            ok
    end,
    State = #state{},
    {ok, State}.

handle_call(_Request, _From, State) ->
    ?INFO("Undefined request:~p", [_Request]),
    {noreply, State}.

handle_cast({set_cache, Key, Val}, State) ->
    ets:insert(cache, {Key, Val}),
    {noreply, State};

handle_cast({save_arena_cache, Rs, Lev, Exp, Heroes}, State) ->
    #role{id = Rid, arena_id = Aid, name = Name} = Rs,
    ets:insert(cache, {{arena, Aid}, {0, Rid, Lev, Name, Heroes}}),
    util:test1(),
    IdL = integer_to_list(Rid),
    LevL = integer_to_list(Lev),
    ExpL = integer_to_list(Exp),
    Power = mod_hero:calc_power(Heroes),
    PowerL = integer_to_list(Power),
    Bin = ?ESC_SINGLE_QUOTES(mod_arena:zip_heroes(Heroes)),
    Sql = list_to_binary([
            <<"update arena set lev = ">>, LevL
            ,<<", exp = ">>, ExpL
            ,<<", power = ">>, PowerL
            ,<<", s = '">>, Bin, <<"'">>
            ,<<" where robot = 0 and rid = ">>, IdL
        ]),
    util:test2("gen_arena_data"),
    util:test1(),
    db:execute(Sql),
    util:test2("set_arena_data"),
    {noreply, State};

handle_cast(_Msg, State) ->
    ?INFO("undefined msg:~p", [_Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    ?INFO("undefined info:~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -- 内部函数 --

