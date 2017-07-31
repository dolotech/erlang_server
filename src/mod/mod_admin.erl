%% ------------------------------------------------------------------
%% $Id: mod_admin.erl 5772 2013-12-07 09:34:35Z rolong $
%%
%% 后台管理通信及数据统计进程
%% ------------------------------------------------------------------
-module(mod_admin).
-behaviour(gen_server).
-include("common.hrl").
-include("offline.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-define(SERVER, admin).

-define(ADMIN_TABLE_NAME, admin).
-define(ADMIN_FILE_NAME, "dets/admin").

-record(state, {
        gold_sub     = [] %% [{DayStamp, SpendSum}]
        ,gold_add    = [] %% [{DayStamp, SpendSum}]
        ,diamond_sub = [] %% [{DayStamp, SpendSum}]
        ,diamond_add = [] %% [{DayStamp, SpendSum}]
        ,notices     = [] %% 公告 [{Id, Msg, Start, End}]
    }).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

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

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    ?INFO("start ~w...", [?MODULE]),
    State = init_state(),
    erlang:send_after(60000, self(), loop),
    {ok, State}.

handle_call(save_data, _From, State) ->
    save_state(State),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({gold_sub, Add}, State) ->
    GoldSub = State#state.gold_sub,
    Key = util:unixtime(today),
    Value = util:get_val(Key, GoldSub, 0),
    Value1 = Value + Add,
    GoldSub1 = util:set_val(Key, Value1, GoldSub),
    State1 = State#state{gold_sub = GoldSub1},
    {noreply, State1};

handle_cast({gold_add, Add}, State) ->
    GoldAdd = State#state.gold_add,
    Key = util:unixtime(today),
    Value = util:get_val(Key, GoldAdd, 0),
    Value1 = Value + Add,
    GoldAdd1 = util:set_val(Key, Value1, GoldAdd),
    State1 = State#state{gold_add = GoldAdd1},
    {noreply, State1};

handle_cast({diamond_sub, Add}, State) ->
    DiamondSub = State#state.diamond_sub,
    Key = util:unixtime(today),
    Value = util:get_val(Key, DiamondSub, 0),
    Value1 = Value + Add,
    DiamondSub1 = util:set_val(Key, Value1, DiamondSub),
    State1 = State#state{diamond_sub = DiamondSub1},
    {noreply, State1};

handle_cast({diamond_add, Add}, State) ->
    DiamondAdd = State#state.diamond_add,
    Key = util:unixtime(today),
    Value = util:get_val(Key, DiamondAdd, 0),
    Value1 = Value + Add,
    DiamondAdd1 = util:set_val(Key, Value1, DiamondAdd),
    State1 = State#state{diamond_add = DiamondAdd1},
    {noreply, State1};

handle_cast({send_notice, PidSender}, State) ->
    case get_notice(State) of
        [] -> ok;
        Notice -> sender:pack_send(PidSender, 15005, [Notice])
    end,
    {noreply, State};

handle_cast(Msg, State) ->
    ?WARN("Undefined msg:~w", [Msg]),
    {noreply, State}.

%% 每分钟执行一次
handle_info(loop, State) ->
    {_, {H, M, _}} = erlang:localtime(),
    case (M div 5) =:= 0 of
        true -> save_state(State);
        false -> ok
    end,
    State1 = case H == 12 andalso M == 2 of
        true -> save_economy_stat(State);
        false -> State
    end,
    erlang:send_after(60000, self(), loop),
    %% TODO:定时更新公告，需要定一个合适的时间间隔
    %% 当前测试时为一分钟更新一次
    self() ! update_notice,
    {noreply, State1};

handle_info(update_notice, State) ->
    case get_notice() of
        [] -> {noreply, State};
        Notices ->
            State1 = State#state{notices = Notices},
            Notice = get_notice(State),
            sender:pack_cast(world, 10005, [Notice]),
            {noreply, State1}
    end;

%% TEST
handle_info(test, State) ->
    gen_server:cast(self(), {diamond_add, 101}),
    gen_server:cast(self(), {diamond_sub, 102}),
    gen_server:cast(self(), {gold_add, 103}),
    gen_server:cast(self(), {gold_sub, 104}),
    save_state(State),
    State1 = save_economy_stat(State),
    {noreply, State1};

handle_info({add_item, From, IdType, Id, Tid, Num}, State) ->
    case get_pid(IdType, Id) of
        false -> From ! {error};
        Pid ->
            Ids = case Tid < 1000 of
                true -> get_tids(Tid, Num);
                false ->
                    AllIds = data_equ:get(ids) ++
                    data_prop:get(ids),
                    case lists:member(Tid, AllIds) of
                        true -> [{Tid, Num}];
                        false -> []
                    end
            end,
            case Ids == [] of
                true -> From ! {error};
                false ->
                    Pid ! {pt, 2002, [Ids]},
                    From ! {ok, Tid, Num}
            end
    end,
    {noreply, State};

handle_info(show_data, State) ->
    show_data(),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    save_state(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_pid(IdType, Id) ->
    Id2 = fix_id(IdType, Id),
    lib_role:get_role_pid(IdType, Id2).

fix_id(name, Id) when is_list(Id) -> list_to_bitstring(Id);
fix_id(account_id, Id) when is_list(Id) -> list_to_bitstring(Id);
fix_id(role_id, Id) when is_list(Id) -> list_to_integer(Id);
fix_id(_, Id) -> Id.

get_tids(Sort, Num) ->
    Ids = mod_item:get_ids_by_sort(Sort),
    [{Id, Num} || Id <- Ids].

init_state() ->
    FileName = ?ADMIN_FILE_NAME,
    TableName = ?ADMIN_TABLE_NAME,
    case filelib:is_file(FileName) of
        true ->
            ets:file2tab(FileName),
            ?INFO("Init ~w! [size:~w, memory:~w]",
                [TableName, ets:info(TableName, size), ets:info(TableName, memory)]),
            #state{
                gold_sub     = get_val_from_ets(gold_sub, [])
                ,gold_add    = get_val_from_ets(gold_add, [])
                ,diamond_sub = get_val_from_ets(diamond_sub, [])
                ,diamond_add = get_val_from_ets(diamond_add, [])
            };
        false ->
            ets:new(TableName, [{keypos, 1}, named_table, protected, set]),
            ?INFO("New ets table: ~w!", [TableName]),
            #state{}
    end.

save_state(State) ->
    %% ?INFO("save state ...", []),
    FileName = ?ADMIN_FILE_NAME,
    TableName = ?ADMIN_TABLE_NAME,
    try
        #state{
            gold_sub     = GoldSub
            ,gold_add    = GoldAdd
            ,diamond_sub = DiamondSub
            ,diamond_add = DiamondAdd
        } = State,
        set_val_to_ets(gold_sub   , GoldSub   ),
        set_val_to_ets(gold_add   , GoldAdd   ),
        set_val_to_ets(diamond_sub, DiamondSub),
        set_val_to_ets(diamond_add, DiamondAdd),
        ets:tab2file(TableName, FileName)
    catch
        T:X -> ?ERR("[~p:~p]", [T, X])
    end.

get_val_from_ets(K, DefaultValue) ->
    case ets:lookup(?ADMIN_TABLE_NAME, K) of
        [{_, V}] -> V;
        [] -> DefaultValue;
        Else ->
            ?WARNING("Undefined data: ~w", [Else]),
            DefaultValue
    end.

set_val_to_ets(K, V) ->
    ets:insert(?ADMIN_TABLE_NAME, {K, V}).

show_data() ->
    L = ets:tab2list(?ADMIN_TABLE_NAME),
    io:format("~n~w~n", [L]).

save_economy_stat(State) ->
    ?INFO("save_economy_stat ...", []),
    #state{
        gold_sub     = GoldSub
        ,gold_add    = GoldAdd
        ,diamond_sub = DiamondSub
        ,diamond_add = DiamondAdd
    } = State,
    Y = util:unixtime(yesterday),
    GoldSub1    = save_economy_stat1(GoldSub   , "gold_sub"   , Y, []),
    GoldAdd1    = save_economy_stat1(GoldAdd   , "gold_add"   , Y, []),
    DiamondSub1 = save_economy_stat1(DiamondSub, "diamond_sub", Y, []),
    DiamondAdd1 = save_economy_stat1(DiamondAdd, "diamond_add", Y, []),
    State#state{
        gold_sub     = GoldSub1
        ,gold_add    = GoldAdd1
        ,diamond_sub = DiamondSub1
        ,diamond_add = DiamondAdd1
    }.

save_economy_stat1([{DayStamp, Val} | T], Type, Yesterday, Rt) ->
    case DayStamp =< Yesterday of
        true ->
            ?INFO("save_economy_stat1 ...", []),
            SelectSql = "SELECT count(*) FROM `log_economy` WHERE day_stamp = ~s",
            case db:get_one(SelectSql, [DayStamp]) of
                0 ->
                    {{Y, M, _}, _} = util:mktime({to_date, DayStamp}),
                    MonDate = Y * 100 + M,
                    InsertSql = "INSERT INTO `log_economy`(`day_stamp`, `mon_date`, `"++Type++"`) VALUES (~s, ~s, ~s)",
                    case db:execute(InsertSql, [DayStamp, MonDate, Val]) of
                        {error, _} ->
                            save_economy_stat1(T, Type, Yesterday, [{DayStamp, Val} | Rt]);
                        {ok, _} ->
                            save_economy_stat1(T, Type, Yesterday, Rt)
                    end;
                1 ->
                    UpdateSql = "UPDATE `log_economy` SET `"++Type++"`=~s WHERE `day_stamp` = ~s",
                    case db:execute(UpdateSql, [Val, DayStamp]) of
                        {error, _} ->
                            save_economy_stat1(T, Type, Yesterday, [{DayStamp, Val} | Rt]);
                        {ok, _} ->
                            save_economy_stat1(T, Type, Yesterday, Rt)
                    end;
                E ->
                    ?WARN("save_economy_stat1 error: ~w", [E]),
                    save_economy_stat1(T, Type, Yesterday, Rt)
            end;
        false ->
            save_economy_stat1(T, Type, Yesterday, [{DayStamp, Val} | Rt])
    end;
save_economy_stat1([], _Type, _Yesterday, Rt) ->
    Rt.

get_notice() ->
    try
        Now = util:unixtime(),
        case db:get_all("SELECT `id`, `msg`, `start_time`, `end_time` FROM `sys_notices` WHERE `end_time` > ~s;", [Now]) of
            {ok, Data} ->
                %% ?INFO("update_notice, count:~w", [length(Data)]),
                [{Id, Msg, StartTime, EndTime} || [Id, Msg, StartTime, EndTime] <- Data];
            {error, null} ->
                [];
            {error, Reason} ->
                ?WARN("Error When get_notice: ~w", [Reason]),
                []
        end
        catch T:X ->
            ?ERR("~w:~w", [T, X]),
            []
    end.

get_notice(State) ->
    Now = util:unixtime(),
    [{Id, Msg} || {Id, Msg, S, E} <- State#state.notices, Now > S, Now < E].
