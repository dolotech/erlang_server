%%----------------------------------------------------
%% 排行榜服务
%%
%% $Id$
%%
%% @author Rolong<rolong@vip.qq.com>
%%----------------------------------------------------
-module(mod_luck).
-behaviour(gen_server).
-export([
        start_link/0
		,update_luck_id/0
		,update_rank/0
		%% ,update_luck_list/0
		%% ,update_luck_week_list/0
		,get_luck_id/0
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
-define(SERVER, luck).
-record(state, {
    }
).

%% 排行榜数据
-record(myrank_data,
    {
        id              = 0     %% 角色ID
        ,name           = <<>>  %% 角色名字
        ,use_sum        = 0
        ,val_sum        = 0
        ,reward_id      = 0
        ,reward_num     = 0
        ,ctime          = 0
    }
).

%% --- 对外接口 ---

%% 新建连接
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% --- 服务器内部实现 ---

init([]) ->
    ?INFO("start ~w...~n", [?MODULE]),
    State = #state{},
    set_rank_from_db(),
    set_recent_from_db(),
	set_week_from_db(),
    put(rank_data, []),
	put(luck_id, 1),
    {ok, State}.

handle_call(save, _From, State) ->
    permanent_stores(),
    {reply, ok, State};

%% 获取期号
handle_call({get_luck_id}, _From, State) ->
    Id = get(luck_id),
    {reply, Id, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

%% 获取排名列表
handle_info({rank_list, PidSender}, State) ->
    Data = get(rank),
    sender:pack_send(PidSender, 13044, [Data]),
	?INFO("Data:~w", [Data]),
    {noreply, State};

handle_info({recent_list, PidSender}, State) ->
    Data = get(recent),
    sender:pack_send(PidSender, 13042, [Data]),
    ?INFO("recent_list:~w", [Data]),
    {noreply, State};

handle_info({week_rank_list, PidSender}, State) ->
    Data = get(week_rank),
    sender:pack_send(PidSender, 13044, [Data]),
	?INFO("Data:~w", [Data]),
	%% ?INFO("week_rank_list",[]),
    {noreply, State};

%% 更新排行榜
handle_info(update_rank, State) ->
    case permanent_stores() of
        ok ->
            ?INFO("update_luck_rank.....", []),
            set_rank_from_db(),
			set_week_from_db(),
            ok;
        _ ->
            ok
    end,
    {noreply, State};

%% 更新排行榜
handle_info(update_luck_id, State) ->
	%% 幸运星 期号
	Ids = data_luck:get(ids),
	Id = get(luck_id),	%% 期号
	case Id + 1 > length(Ids) of
		true ->
			put(week_rank_list,[]),
			put(luck_id, 1);
		false ->
			put(week_rank_list,[]),
			put(luck_id, Id + 1)
	end,
	?INFO("Id:~w", [Id]),
	{noreply, State};

%% %% 更新总排行榜
%% handle_info(update_luck_list, State) ->
%% 	set_rank_from_db(),
%% 	{noreply, State};
%%
%% %% 更新周排行榜
%% handle_info(update_luck_week_list, State) ->
%% 	set_week_from_db(),
%% 	{noreply, State};

handle_info(test, State) ->
    ?INFO("DICT:~p", [get()]),
    {noreply, State};

handle_info(_Info, State) ->
    ?ERR("Not matched info: ~w", [_Info]),
    {noreply, State}.

handle_cast({set_myrank, Rid, Name, UseSum, ValSum, RewardId, RewardNum, Quality}, State) ->
    ?INFO("set_myrank:~w", [[Rid, Name, UseSum, ValSum, RewardId, RewardNum, Quality]]),
    set_myrank_data(Rid, Name, UseSum, ValSum, RewardId, RewardNum, Quality),
    {noreply, State};

handle_cast(_Msg, State) ->
    ?ERR("Not matched message: ~w", [_Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --- 私有函数 ---

%% 更新角色信息中的数据 从进程字典中
set_myrank_data(Rid, Name, UseSum, ValSum, RewardId, RewardNum, Quality) ->
    RankData = get(rank_data),
    Ctime = util:unixtime(),
	?INFO("RankData:~w", [RankData]),
    R1 = case lists:keyfind(Rid, 2, RankData) of
        false ->
            #myrank_data{
                id              = Rid
                ,name           = Name
                ,use_sum        = UseSum
                ,val_sum        = ValSum
                ,reward_id      = RewardId
                ,reward_num     = RewardNum
                ,ctime          = Ctime
            };
        R ->
            R#myrank_data{
                name            = Name
                ,use_sum        = UseSum
                ,val_sum        = ValSum
                ,reward_id      = RewardId
                ,reward_num     = RewardNum
                ,ctime          = Ctime
            }
    end,
    NewRankData = lists:keystore(Rid, 2, RankData, R1),
    put(rank_data, NewRankData),
	case Quality =:= 2 of
        true ->
            Recent = get(recent),
            Recent1 = lists:sublist(Recent, 1, 19),
            Recent2 = [[Name, RewardId, RewardNum] | Recent1],
            put(recent, Recent2),
            ok;
        false -> ok
    end.

%% 更新期号
update_luck_id() ->
	?SERVER ! update_luck_id.

%% 更新排行榜
update_rank() ->
	?SERVER ! update_rank.

%% %% 更新总排行榜
%% update_luck_list() ->
%% 	permanent_stores(),
%% 	?SERVER ! update_luck_list.
%%
%% %% 更新周排行榜
%% update_luck_week_list() ->
%% 	permanent_stores(),
%% 	?SERVER ! update_luck_week_list.

%% 获取期号
get_luck_id() ->
	gen_server:call(?SERVER, {get_luck_id}).

%% 添加排名字段
%% add_rank_pos(Data) ->
%%     add_rank_pos(Data, 0, []).
%% add_rank_pos([], _, Rs) ->
%%     lists:reverse(Rs);
%% add_rank_pos([R|T], Key, Rs) ->
%%     Pos = Key + 1,
%%     add_rank_pos(T, Pos, [[Pos|R]|Rs]).

permanent_stores() ->
    RankData = get(rank_data),
    ?INFO("count luck rank_data:~w~n", [length(RankData)]),
    permanent_stores(RankData).

permanent_stores([]) ->
    put(rank_data, []),
    ok;
permanent_stores([R|T]) ->
    #myrank_data{
        id              = Id
        ,name           = Name
        ,use_sum        = UseSum
        ,val_sum        = ValSum
        ,reward_id      = RewardId
        ,reward_num     = RewardNum
        ,ctime          = Ctime
    } = R,
    Sql = "UPDATE `rank_luck` SET `name` = ~s, `use_sum` = ~s,"
    " `val_sum` = ~s, `reward_id` = ~s, `reward_num` = ~s,"
    " `ctime` = ~s WHERE `id` =  ~s;",
    case db:execute(Sql, [Name, UseSum, ValSum, RewardId, RewardNum, Ctime, Id]) of
        {ok, _} ->
			?INFO("Name:~s", [Name]),
            Sql2 = "INSERT INTO `rank_luck` "
            "(`id`, `name`, `use_sum`, `val_sum`, `reward_id`, `reward_num`, `ctime`) "
            "VALUES (~s, ~s, ~s, ~s, ~s, ~s, ~s);",
            db:execute(Sql2, [Id, Name, UseSum, ValSum, RewardId, RewardNum, Ctime]),
            permanent_stores(T);
        {error, Reason} ->
            put(rank_data, [R|T]),
            ?WARN("~w", [Reason]),
            error;
        _ ->
            permanent_stores(T)
    end.

set_rank_from_db() ->
    %% Sql = lists:concat(["select "
    %%         "id, name, use_sum, val_sum "
    %%         "from rank_luck order by val_sum desc limit 0, 100"]),
    %% ?INFO("Sql:~p",[Sql]),
	Sql = lists:concat(["select "
					   "name, use_sum, val_sum "
					   "from rank_luck order by val_sum desc limit 0, 100"]),
    case db:get_all(Sql) of
        {ok, Data} -> put(rank, Data);
        _ -> put(rank, [])
    end.

set_recent_from_db() ->
    %% Sql = lists:concat(["select "
    %%         "id, name, reward_id, reward_num "
    %%         "from rank_luck order by ctime desc limit 0, 20"]),
    %% ?INFO("Sql:~p",[Sql]),
	Sql = lists:concat(["select "
					   "name, reward_id, reward_num "
					   "from rank_luck order by ctime desc limit 0, 50"]),
    case db:get_all(Sql) of
        {ok, Data} -> put(recent, Data);
        _ -> put(recent, [])
    end.

set_week_from_db() ->
	Sql = lists:concat(["select "
					   "name, use_sum, val_sum "
					   "from rank_luck order by val_sum desc limit 0, 100"]),
    case db:get_all(Sql) of
        {ok, Data} -> put(week_rank, Data);
        _ -> put(week_rank, [])
    end.

%% data文件中的数据：
%%
%% -module(data_luck).
%% -export([get/1]).
%%
%% get(1) ->
%%     [{170001, 1, 200, {1,27200}},
%%      {240019, 1, 1000, {27201,30200}},
%%      {290002, 1, 300, {30201,45200}},
%%      {4, 1000, 10000, {45201,45500}},
%%      {1, 2000, 100, {45501,75500}},
%%      {170003, 1, 1800, {75501,79500}},
%%      {110044, 1, 5000, {79501,80000}},
%%      {220002, 1, 300, {80001,100000}}];
%%
%% get(Key) ->
%%     io:format("\n[~w:~w] Key(~w) undefined!\n", [?MODULE, ?LINE, Key]),
%%     [].

%% 以下是handle代码：
%%
%% 转盘
%% Rs#role.luck = {1000, 0, 0} = {累积的点券,使用物品数量,累计价值}
%% handle(13040, [], Rs) ->
%%     case data_luck:get(1) of
%%         [] -> {ok, [1, 0, 0, 0, 0, 0]};
%%         LuckData ->
%%             SpendTid = 320001,
%%             SpendNum = 1,
%%             MyItems = lib_item:get_myitems(),
%%             #role{id = Rid, name = Name, luck = {CardSum, UseSum, ValSum}} = Rs,
%%             case lib_item:del_item(by_tid, SpendTid, SpendNum, MyItems) of
%%                 {ok, MyItems1, Dels} ->
%%                     Rand = util:rand(1, 100000),
%%                     case luck_process(Rand, LuckData) of
%%                         {1, Num, Val} ->
%%                             Rs1 = lib_role:add_attr(Rs, gold, Num, 1005),
%%                             CardSum1 = CardSum + 25,
%%                             UseSum1 = UseSum + 1,
%%                             ValSum1 = ValSum + Val,
%%                             Luck1 = {CardSum1, UseSum1, ValSum1},
%%                             Rs2 = Rs1#role{luck = Luck1},
%%                             lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
%%                             gen_server:cast(mod_luck, {set_myrank, Rid, Name, Val,
%%                                     CardSum1, UseSum1, ValSum1, 1, Num}),
%%                             lib_item:put_myitems(MyItems1),
%%                             lib_conn:pack_send(Rs#role.pid_sender, 17109, [Dels]),
%%                             {ok, [0, CardSum1, UseSum1, ValSum1, 1, Num], Rs2};
%%                         {2, Num, Val} ->
%%                             Rs1 = lib_role:add_attr(Rs, card, Num, 1005),
%%                             CardSum1 = CardSum + 25,
%%                             UseSum1 = UseSum + 1,
%%                             ValSum1 = ValSum + Val,
%%                             Luck1 = {CardSum1, UseSum1, ValSum1},
%%                             Rs2 = Rs1#role{luck = Luck1},
%%                             lib_conn:pack_send(Rs#role.pid_sender, 11020, [1]),
%%                             gen_server:cast(mod_luck, {set_myrank, Rid, Name, Val,
%%                                     CardSum1, UseSum1, ValSum1, 2, Num}),
%%                             lib_item:put_myitems(MyItems1),
%%                             lib_conn:pack_send(Rs#role.pid_sender, 17109, [Dels]),
%%                             {ok, [0, CardSum1, UseSum1, ValSum1, 2, Num], Rs2};
%%                         {4, Num, Val} ->
%%                             Rs1 = lib_role:add_attr(Rs, card, CardSum, 1006),
%%                             CardSum1 = 1000,
%%                             UseSum1 = UseSum + 1,
%%                             ValSum1 = ValSum + Val,
%%                             Luck1 = {CardSum1, UseSum1, ValSum1},
%%                             Rs2 = Rs1#role{luck = Luck1},
%%                                             CardSum1, UseSum1, ValSum1, RewardTid, Num}),
%%                                     {ok, [0, CardSum1, UseSum1, ValSum1, RewardTid, Num], Rs2};
%%                                 {error, at_full} ->
%%                                     %% lib_conn:send_code(Rs#role.pid_sender, 17000101),
%%                                     {ok, [3, CardSum, UseSum, ValSum, 0, 0]};
%%                                 {error, Reason} ->
%%                                     ?ERR("error: ~w", [Reason]),
%%                                     {ok, [1, CardSum, UseSum, ValSum, 0, 0]}
%%                             end
%%                     end;
%%                 {error, _} -> {ok, [2, CardSum, UseSum, ValSum, 0, 0]}
%%             end
%%     end;
%%
%% handle(13042, [], Rs) ->
%%     {CardSum, UseSum, ValSum} = Rs#role.luck,
%%     luck ! {recent_list, CardSum, UseSum, ValSum,
%%         Rs#role.pid_sender},
%%     {ok};
%%
%% handle(13044, [], Rs) ->
%%     luck ! {rank_list, Rs#role.pid_sender},
%%     {ok};
%%
%% 转盘
%% luck_process(Rand, [{Id, Num, Value, {Min, Max}} | T]) ->
%%     case Rand >= Min andalso Rand =< Max of
%%         true -> {Id, Num, Value};
%%         false -> luck_process(Rand, T)
%%     end;
%% luck_process(_, []) -> {0, 0, 0}.
%%
