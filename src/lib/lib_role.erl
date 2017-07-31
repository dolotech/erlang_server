%% ------------------------------------------------------------------
%% 角色相关API库
%%
%% $Id: lib_role.erl 5772 2013-12-07 09:34:35Z rolong $
%% @author Rolong<rolong@vip.qq.com>
%% ------------------------------------------------------------------
-module(lib_role).
-export([
    get_role_pid/2
    ,spend/3
    ,spend_ok/4
    ,add_attr/2
    ,add_attr/3
    ,add_attr_ok/4
    ,get_role_pid_from_ets/2
    ,get_online_pid/2
    ,get_offline_pid/2
    ,get_new_id/2
    ,stop/0
    ,init_skills/1
    ,exec_cmd/1
    ,time2power/1
    ,notice/1
    ,notice/2
    ,notice/3
    ,db_init/3
    ,db_update/1
    ,zip_kvs/1
    ]).

-include("common.hrl").
-include("offline.hrl").

%%' 消耗属性
-spec spend(MoneyType, Value, Rs) -> {ok, NewRs} | {error, Reason} when
    MoneyType :: gold | diamond, %% 货币类型
    Value     :: integer(),      %% 消耗数量
    Rs        :: #role{},        %% 消耗前的状态
    NewRs     :: #role{},        %% 消耗后的状态
    Reason    :: term().         %% 错误信息

%% 消耗金币
spend(gold, 0, R) -> {ok, R};
spend(gold, V, R) when V > 0 ->
    V1 = R#role.gold - V,
    case V1 >= 0 of
        true -> {ok, R#role{gold = V1}};
        false -> {error, gold}
    end;

%% 消耗钻石
spend(diamond, 0, R) -> {ok, R};
spend(diamond, V, R) when V > 0 ->
    V1 = R#role.diamond - V,
    case V1 >= 0 of
        true -> {ok, R#role{diamond = V1}};
        false -> {error, diamond}
    end;

spend(Type, V, _R) ->
    ?ERR("ERROR TYPE: ~w, VALUE: ~w", [Type, V]),
    {error, Type}.
%%.

%%' 增加属性
-spec add_attr(MoneyType, Value, Rs) -> NewRs when
    MoneyType :: gold | diamond, %% 货币类型
    Value     :: integer(),      %% 消耗数量
    Rs        :: #role{},        %% 消耗前的状态
    NewRs     :: #role{}.        %% 消耗后的状态

%% 增加金币
add_attr(gold, V, R) when V =< 0 -> R;
add_attr(gold, V, R) ->
    R#role{gold = R#role.gold + V};

%% 增加钻石
add_attr(diamond, V, R) when V =< 0 -> R;
add_attr(diamond, V, R) ->
    R#role{diamond = R#role.diamond + V};

%% 增加幸运星
add_attr(luck, V, R) when V =< 0 -> R;
add_attr(luck, V, R) ->
	{LuckStar, A, B, C} = R#role.luck,
	NewStar = {LuckStar + V, A, B, C},
	R#role{luck = NewStar};

add_attr(K, V, R) ->
    ?ERR("[add_attr] undefined kvs: ~w-~w", [K, V]),
    R.
%%.

%%' 批量增加属性
-spec add_attr(Attrs, Rs) -> NewRs when
    Attrs :: [{AttrType, Value}],
    AttrType :: gold | diamond,
    Value :: integer(),
    Rs :: #role{},
    NewRs :: #role{}.

add_attr([{K, V}|T], R) ->
    NewR = add_attr(K, V, R),
    add_attr(T, NewR);
add_attr([], R) -> R.
%%.

%%' 成功消耗属性
-spec spend_ok(MoneyType, LogType, RsA, RsB) -> NewRsB when
    MoneyType :: gold | diamond, %% 货币类型
    LogType   :: integer(),      %% 日志类型（由策划定义）
    RsA       :: #role{},        %% 消耗前的状态
    RsB       :: #role{},        %% 消耗后的状态
    NewRsB    :: #role{}.        %% 返回新的状态

spend_ok(gold, LogType, RsA, RsB) ->
    V = RsA#role.gold - RsB#role.gold,
    %% 金币消耗统计
    gen_server:cast(admin, {gold_sub, V}),
    %% 日志
    ?LOG({gold, RsA#role.id, LogType, -V, RsB#role.gold}),
    RsB;
spend_ok(diamond, LogType, RsA, RsB) ->
    V = RsA#role.diamond - RsB#role.diamond,
    %% 钻石消耗统计
    gen_server:cast(admin, {diamond_sub, V}),
    %% 日志
    ?LOG({diamond, RsA#role.id, LogType, -V, RsB#role.diamond}),
    RsB;
spend_ok(Type, _LogType, _RsA, RsB) ->
    ?ERR("ERROR TYPE: ~w", [Type]),
    RsB.
%%.

%%' 成功增加属性
-spec add_attr_ok(MoneyType, LogType, RsA, RsB) -> NewRsB when
    MoneyType :: gold | diamond, %% 货币类型
    LogType   :: integer(),      %% 日志类型（由策划定义）
    RsA       :: #role{},        %% 增加前的状态
    RsB       :: #role{},        %% 增加后的状态
    NewRsB    :: #role{}.        %% 返回新的状态

add_attr_ok(gold, LogType, RsA, RsB) ->
    V = RsB#role.gold - RsA#role.gold,
    %% 金币产出统计
    gen_server:cast(admin, {gold_add, V}),
	%% 金币成就推送
	RsB2 = mod_attain:attain_state(31, V, RsB),
    %% 日志
    ?LOG({gold, RsA#role.id, LogType, V, RsB#role.gold}),
    RsB2;

add_attr_ok(diamond, LogType, RsA, RsB) ->
    V = RsB#role.diamond - RsA#role.diamond,
    %% 钻石产出统计
    gen_server:cast(admin, {diamond_add, V}),
    %% 日志
    ?LOG({diamond, RsA#role.id, LogType, V, RsB#role.diamond}),
    RsB;
%% add_attr_ok(luck, _LogType, RsA, RsB) ->
%% 	{LuckStar1, _, _, _} = RsB#role.luck,
%% 	{LuckStar2, _, _, _} = RsA#role.luck,
%%     V = LuckStar1 - LuckStar2,
%%     %% 幸运星产出统计
%%     gen_server:cast(admin, {luck_add, V}),
%%     RsB;
add_attr_ok(Type, _LogType, _RsA, RsB) ->
    ?ERR("ERROR TYPE: ~w", [Type]),
    RsB.
%%.


notice(Rs) ->
    #role{pid_sender = Sender,
          diamond = Diamond, gold = Gold} = Rs,
    sender:pack_send(Sender, 11006, [Diamond]),
    sender:pack_send(Sender, 11005, [Gold]).

notice(honor, Rs) ->
    #role{pid_sender = Sender, arena_honor = V} = Rs,
    sender:pack_send(Sender, 11004, [V]);

notice(luck, Rs) ->
	{LuckStar, _, _, _} = Rs#role.luck,
    #role{pid_sender = Sender} = Rs,
    sender:pack_send(Sender, 11008, [LuckStar]);

notice(diamond, Rs) ->
    #role{pid_sender = Sender, diamond = V} = Rs,
    sender:pack_send(Sender, 11006, [V]);
notice(gold, Rs) ->
    #role{pid_sender = Sender, gold = V} = Rs,
    sender:pack_send(Sender, 11005, [V]).

notice(diamond, Sender, V) ->
    sender:pack_send(Sender, 11006, [V]);
notice(gold, Sender, V) ->
    sender:pack_send(Sender, 11005, [V]).

time2power(Rs) ->
    #role{power = Power,
         power_time = PowerTime} = Rs,
    Max = data_config:get(tired_max),
    CurTime = util:unixtime(),
    case Power >= Max of
        true ->
            Rs#role{power_time = CurTime};
        false ->
            T = util:ceil(1440/Max*4*60),
            Sec = case PowerTime > 0 of
                             true -> CurTime - PowerTime;
                             false -> 0
                         end,
            Add = Sec div T,
            case Add > 0 of
                true ->
                    Rest = Sec rem T,
                    PowerTime1 = CurTime - Rest,
                    Rs#role{power = Power + Add,
                           power_time = PowerTime1};
                false ->
                    Rs#role{power_time = CurTime}
            end
    end.

exec_cmd([{M, F, A} | T]) ->
    erlang:apply(M, F, A),
    exec_cmd(T);
exec_cmd([]) ->
    ok.

stop() ->
    ?INFO("logout ", []),
    L = ets:tab2list(online),
    do_stop_all(L),
    L2 = ets:tab2list(offline),
    do_stop_all2(L2).

do_stop_all([]) ->
    util:sleep(1000),
    L = ets:tab2list(online),
    Len = length(L),
    case Len > 0 of
        true ->
            io:format("Rest: ~w!", [Len]),
            util:sleep(5000),
            %% do_stop_all([]);
            ok;
        false ->
            %% io:format("done!", []),
            ok
    end;
do_stop_all([H | T]) ->
    H#online.pid ! shutdown,
    io:format("."),
    util:sleep(100),
    do_stop_all(T).

do_stop_all2([]) ->
    util:sleep(1000),
    L = ets:tab2list(offline),
    Len = length(L),
    case Len > 0 of
        true ->
            io:format("Rest2: ~w!", [Len]),
            util:sleep(5000),
            %% do_stop_all2([]);
            ok;
        false ->
            io:format("done!", []),
            ok
    end;
do_stop_all2([H | T]) ->
    H#offline.pid ! shutdown,
    io:format("."),
    util:sleep(10),
    do_stop_all2(T).

get_new_id(hero, Rs) ->
    Id = Rs#role.max_hero_id + 1,
    {ok, Id, Rs#role{max_hero_id = Id}};
get_new_id(item, Rs) ->
    Id = Rs#role.max_item_id + 1,
    {ok, Id, Rs#role{max_item_id = Id}}.

%% init_role_data(Key, Val, Rs) ->
%%     {K, V} = case Key of
%%         account_id -> {<<"aid">>, ?ESC_SINGLE_QUOTES(Val)};
%%         role_id -> {<<"id">>, integer_to_list(Val)};
%%         name -> {<<"name">>, ?ESC_SINGLE_QUOTES(Val)}
%%     end,
%%     Sql = list_to_binary([
%%             <<"select gold, diamond, essence, lev, tollgate_id, kvs from role where ">>
%%             ,K,<<" = '">>,V,<<"'">>
%%         ]),
%%     ?INFO("Sql:~s", [Sql]),
%%     Data = db:get_row(Sql),
%%     init_role_data2(Data, Rs).
%%
%% init_role_data2([], _Rs) -> false;
%% init_role_data2([Gold, Diamond, Essence, Lev, TollgateId, KvsBin], Rs) ->
%%     Rs1 = Rs#role{
%%         id = Rid
%%         ,account_id = AccountId
%%         ,name = Name
%%     },
%%     Rs2 = mod_role:db_init(Rs1),
%%     Rs3 = mod_hero:db_init(Rs2),
%%     Rs0 = mod_item:db_init(Rs3),
%%     {ok, Rs0}.
%%
%% init_kvs(Rs, <<>>) -> Rs;
%% init_kvs(Rs, KvsBin) ->
%%     Kvs = util:bitstring_to_term(KvsBin),
%%     init_kvs(Kvs, Rs).

%% 获取角色PID
get_role_pid(role_id, 0) -> ?WARN("role_id=0", []), false;
get_role_pid(KeyType, Key) ->
    case get_role_pid_from_ets(KeyType, Key) of
        false ->
            case KeyType of
                name ->
                    RoleId = get_rid(Key),
                    get_role_pid(role_id, RoleId);
                _ ->
                    case role:create(offline, KeyType, Key) of
                        {ok, Pid} -> Pid;
                        _Else ->
                            ?WARN("~w", [_Else]),
                            false
                    end
            end;
        {_, Pid} -> Pid
    end.

%% -> false | {TableName, Pid}
get_role_pid_from_ets(Type, Id) ->
    case get_online_pid(Type, Id) of
        false ->
            case get_offline_pid(Type, Id) of
                false -> false;
                Pid -> {offline, Pid}
            end;
        Pid -> {online, Pid}
    end.

%% 离线表中查找
get_offline_pid(account_id, AccountId) ->
    case ets:lookup(offline, AccountId) of
        [] ->
            false;
        [R] ->
            Pid = R#offline.pid,
            case is_process_alive(Pid) of
                true ->
                    ?DEBUG("from offline:~w", [Pid]),
                    Pid ! {set_stop_timer, ?OFFLINE_CACHE_TIME},
                    Pid;
                false ->
                    ?WARN("Not alive pid in offline table, Rid:~w, Pid: ~w",
                        [R#offline.id, Pid]),
                    ets:delete(offline, AccountId),
                    false
            end
    end;
get_offline_pid(role_id, Rid) ->
    %% 离线表中查找
    MatchSpec = [{
            #offline{pid='$1', id='$2', account_id='$3', _='_'}
            ,[{'==','$2',Rid}]
            ,[['$1', '$3']]
        }],
    case ets:select(offline, MatchSpec) of
        [] ->
            false;
        [[Pid, AccountId]] ->
            case is_process_alive(Pid) of
                true ->
                    ?DEBUG("from offline:~w", [Pid]),
                    Pid;
                false ->
                    ?WARNING("Not alive pid in offline table, Rid:~w, Pid: ~w",
                        [Rid, Pid]),
                    ets:delete(offline, AccountId),
                    false
            end;
        _Else ->
            ?WARNING("unexpected data:~w", [_Else]),
            false
    end;
get_offline_pid(name, Name) ->
    %% 离线表中查找
    MatchSpec = [{
            #offline{pid='$1', name='$2', account_id='$3', id='$4'}
            ,[{'==','$2',Name}]
            ,[['$1', '$3', '$4']]
        }],
    case ets:select(offline, MatchSpec) of
        [] -> false;
        [[Pid, AccountId, Rid]] ->
            case is_process_alive(Pid) of
                true ->
                    ?DEBUG("from offline:~w", [Pid]),
                    Pid;
                false ->
                    ?WARNING("Not alive pid in offline table, Rid:~w, Pid: ~w",
                        [Rid, Pid]),
                    ets:delete(offline, AccountId),
                    false
            end;
        _Else ->
            ?WARNING("unexpected data:~w", [_Else]),
            false
    end.

%% 在线表中查找角色Pid
get_online_pid(account_id, AccountId) ->
    MatchSpec = [{
            #online{pid='$1', account_id='$2', id='$3', _ = '_'}
            ,[{'==','$2',AccountId}]
            ,[['$1', '$3']]
        }],
    case ets:select(online, MatchSpec) of
        [] ->
            false;
        [[Pid, Rid]] ->
            case is_process_alive(Pid) of
                true ->
                    ?DEBUG("from online:~w", [Pid]),
                    Pid;
                false ->
                    ?WARNING("Not alive pid in online table, Rid:~w, Pid: ~w", [Rid, Pid]),
                    ets:delete(online, Rid),
                    false
            end;
        _Else ->
            ?WARNING("unexpected data: ~w", [_Else]),
            false
    end;
get_online_pid(role_id, Rid) ->
    case ets:lookup(online, Rid) of
        [] ->
            false;
        [R] ->
            #online{pid = Pid} = R,
            case is_process_alive(Pid) of
                true ->
                    ?DEBUG("from online:~w", [Pid]),
                    Pid;
                false ->
                    ?WARNING("Not alive pid in online table, Rid:~w, Pid: ~w", [Rid, Pid]),
                    ets:delete(online, Rid),
                    false
            end
    end;
get_online_pid(name, Name) ->
    MatchSpec = [{
            #online{pid='$1', name='$2', id='$3', _ = '_'}
            ,[{'==','$2',Name}]
            ,[['$1', '$3']]
        }],
    case ets:select(online, MatchSpec) of
        [] ->
            false;
        [[Pid, Rid]] ->
            case is_process_alive(Pid) of
                true ->
                    ?DEBUG("from online:~w", [Pid]),
                    Pid;
                false ->
                    ?WARNING("Not alive pid in online table, Rid:~w, Pid: ~w", [Rid, Pid]),
                    ets:delete(online, Rid),
                    false
            end;
        _Else ->
            ?WARNING("unexpected data: ~w", [_Else]),
            false
    end.

init_skills(Data) ->
    Skill1   = util:get_val(skill1    , Data, 0),
    Skill2   = util:get_val(skill2    , Data, 0),
    Skill3   = util:get_val(skill3    , Data, 0),
    [S || S <- [Skill1, Skill2, Skill3], S > 0].

get_rid(Name) when is_list(Name) ->
    Sql = lists:concat(["select id from role where name like '", Name, "' limit 10"]),
    get_rid1(Sql);

get_rid(Name1) when is_binary(Name1) ->
    Name = binary_to_list(Name1),
    Sql = lists:concat(["select id from role where name like '", Name, "' limit 10"]),
    get_rid1(Sql);

get_rid(Aid) when is_integer(Aid) ->
    Sql = lists:concat(["select id from role where aid = '", Aid, "' limit 10"]),
    get_rid1(Sql);

get_rid({aid, Aid}) ->
    Sql = lists:concat(["select id from role where aid = '", Aid, "' limit 10"]),
    get_rid1(Sql).

get_rid1(Sql) ->
    {ok, Data} = db:get_all(Sql),
    get_rid2(Data).

get_rid2([[Id]]) -> Id;
get_rid2([]) -> 0;
get_rid2(Data) ->
    ?WARNING("unexpected data when get_rid/1. (~w)]", [Data]),
    0.

%%' 数据库相关操作

-spec db_init(Key, Val, Rs) -> {ok, NewRs} | {error, Reason} when
    Key :: account_id | role_id | name,
    Val :: integer() | binary(),
    Rs :: NewRs,
    NewRs :: #role{},
    Reason :: term().

db_init(Key, Val, Rs) ->
    {K, V} = case Key of
        account_id -> {<<"aid">>, ?ESC_SINGLE_QUOTES(Val)};
        role_id -> {<<"id">>, integer_to_list(Val)};
        name -> {<<"name">>, ?ESC_SINGLE_QUOTES(Val)}
    end,
    Sql = list_to_binary([
            <<"select id, aid, name, gold, diamond, essence, lev, tollgate_id, kvs from role where ">>
            ,K,<<" = '">>,V,<<"'">>
        ]),
    ?DEBUG("Sql:~s", [Sql]),
    case db:get_row(Sql) of
        {error, null} -> {error, no_reg};
        {ok, [Rid, AccountId, Name, Gold, Diamond, Essence, Lev, TollgateId, KvsBin]} ->
            Rs1 = unzip_kvs(KvsBin, Rs),
            Rs2 = Rs1#role{
                id = Rid
                ,account_id = AccountId
                ,name = Name
                ,lev = Lev
                ,gold = Gold
                ,diamond = Diamond
                ,essence = Essence
                ,tollgate_id = TollgateId
            },
            db_init1(hero, Rs2)
    end.

db_init1(hero, Rs) ->
    case mod_hero:db_init(Rs) of
        {ok, Rs1} -> db_init1(item, Rs1);
        {error, Reason} -> {error, Reason}
    end;
db_init1(item, Rs) ->
    case mod_item:db_init(Rs) of
        {ok, Rs1} -> db_init1(arena, Rs1);
        {error, Reason} -> {error, Reason}
    end;
db_init1(arena, Rs) ->
    case mod_arena:db_init(Rs) of
        {ok, Rs1} -> db_init1(arena2, Rs1);
        {error, Reason} -> {error, Reason}
    end;
db_init1(arena2, Rs) ->
    case mod_arena:db_init2(Rs) of
        {ok, Rs1} -> db_init1(arena_init, Rs1);
        {error, Reason} -> {error, Reason}
    end;
db_init1(arena_init, Rs) ->
    case mod_arena:arena_init(Rs) of
        {ok, Rs1} -> db_init1(attain, Rs1);
        {error, Reason} -> {error, Reason}
    end;
db_init1(attain, Rs) ->
    mod_attain:attain_init(Rs).

-spec db_update(Rs) -> Result when
    Rs :: #role{},
    Result :: integer() | {error, Reason},
    Reason :: term().

db_update(Rs) ->
    mod_arena:db_update(Rs),
    #role{
        id = Rid
        ,lev = Lev
        ,gold = Gold
        ,diamond = Diamond
        ,essence = Essence
        ,tollgate_id = TollgateId
    } = Rs,
    KvsBin = ?ESC_SINGLE_QUOTES(zip_kvs(Rs)),
    Sql = list_to_binary([
            <<"update role set lev = ">>,integer_to_list(Lev)
            ,<<",gold = ">>,integer_to_list(Gold)
            ,<<",diamond = ">>,integer_to_list(Diamond)
            ,<<",essence = ">>,integer_to_list(Essence)
            ,<<",tollgate_id = ">>,integer_to_list(TollgateId)
            ,<<",kvs = '">>,KvsBin,<<"'">>
            ,<<" where id = ">>,integer_to_list(Rid)
        ]),
    db:execute(Sql).

-define(KVS_ZIP_VERSION, 7).

zip_kvs(Rs) ->
    #role{
        power = Power
        ,growth = Growth
        ,sign_days = SignDays
        ,sign_time = SignTime
        ,sign_old_days = SignOldDays
		,bag_mat_max   = BagMatMax
		,bag_prop_max  = BagPropMax
		,bag_equ_max   = BagEquMax
		,arena_time		= ArenaTime
		,arena_honor	= ArenaHonor
		,arena_wars		= ArenaWars
		,arena_chance	= ArenaChance
		,arena_rank_box = ArenaBox
		,arena_combat_box1 = ArenaBox1
		,arena_combat_box2 = ArenaBox2
		,arena_revenge	   = ArenaRev
		,arena_prize	   = ArenaPrize
		,luck = {LuckStar, LuckDia, LuckUsed, ValSum}
    } = Rs,
    <<?KVS_ZIP_VERSION:8, Growth:8, Power:16, SignDays:8, SignTime:32, SignOldDays:8, BagMatMax:16, BagPropMax:16, BagEquMax:16
	,ArenaTime:32 ,ArenaHonor:32 ,ArenaWars:8 ,ArenaChance:8 ,ArenaBox:8 ,ArenaBox1:8, ArenaBox2:8
	,ArenaRev:8, ArenaPrize:8, LuckStar:32
	,LuckDia:32, LuckUsed:32, ValSum:32>>.

unzip_kvs(<<>>, Rs) -> Rs;
unzip_kvs(Bin, Rs) ->
    <<Version:8, Bin1/binary>> = Bin,
    case Version of
        %% 1 ->
        %%     <<Power:16>> = Bin1,
        %%     Rs#role{
        %%         power = Power
        %%     };
        %% 2 ->
        %%     <<Power:16, SignDays:8, SignTime:32, SignOldDays:8>> = Bin1,
        %%     Rs#role{
        %%         power = Power
        %%         ,sign_days = SignDays
        %%         ,sign_time = SignTime
        %%         ,sign_old_days = SignOldDays
        %%     };
        3 ->
            <<Growth:8, Power:16, SignDays:8, SignTime:32, SignOldDays:8>> = Bin1,
            Rs#role{
                power = Power
                ,sign_days = SignDays
                ,sign_time = SignTime
                ,sign_old_days = SignOldDays
                ,growth = Growth
            };
		4 ->
            <<Growth:8, Power:16, SignDays:8, SignTime:32, SignOldDays:8, BagMatMax:16, BagPropMax:16, BagEquMax:16>> = Bin1,
            Rs#role{
                power = Power
                ,sign_days = SignDays
                ,sign_time = SignTime
                ,sign_old_days = SignOldDays
                ,growth = Growth
            	,bag_mat_max  = BagMatMax
				,bag_prop_max = BagPropMax
				,bag_equ_max  = BagEquMax
			};
		5 ->
            <<Growth:8, Power:16, SignDays:8, SignTime:32, SignOldDays:8, BagMatMax:16, BagPropMax:16, BagEquMax:16
			,ArenaTime:32 ,ArenaHonor:32 ,ArenaWars:8 ,ArenaChance:8 ,ArenaBox:8 ,ArenaBox1:8, ArenaBox2:8>> = Bin1,
            Rs#role{
                power = Power
                ,sign_days = SignDays
                ,sign_time = SignTime
                ,sign_old_days = SignOldDays
                ,growth = Growth
            	,bag_mat_max  = BagMatMax
				,bag_prop_max = BagPropMax
				,bag_equ_max  = BagEquMax
				,arena_time		= ArenaTime
				,arena_honor	= ArenaHonor
				,arena_wars		= ArenaWars
				,arena_chance	= ArenaChance
				,arena_rank_box = ArenaBox
				,arena_combat_box1 = ArenaBox1
				,arena_combat_box2 = ArenaBox2
			};
		6 ->
            <<Growth:8, Power:16, SignDays:8, SignTime:32, SignOldDays:8, BagMatMax:16, BagPropMax:16, BagEquMax:16
			,ArenaTime:32 ,ArenaHonor:32 ,ArenaWars:8 ,ArenaChance:8 ,ArenaBox:8 ,ArenaBox1:8, ArenaBox2:8
			,ArenaRev:8, ArenaPrize:8>> = Bin1,
            Rs#role{
                power = Power
                ,sign_days = SignDays
                ,sign_time = SignTime
                ,sign_old_days = SignOldDays
                ,growth = Growth
            	,bag_mat_max  = BagMatMax
				,bag_prop_max = BagPropMax
				,bag_equ_max  = BagEquMax
				,arena_time		= ArenaTime
				,arena_honor	= ArenaHonor
				,arena_wars		= ArenaWars
				,arena_chance	= ArenaChance
				,arena_rank_box = ArenaBox
				,arena_combat_box1 = ArenaBox1
				,arena_combat_box2 = ArenaBox2
				,arena_revenge	   = ArenaRev
				,arena_prize	   = ArenaPrize
			};
		7 ->
            <<Growth:8, Power:16, SignDays:8, SignTime:32, SignOldDays:8, BagMatMax:16, BagPropMax:16, BagEquMax:16
			,ArenaTime:32 ,ArenaHonor:32 ,ArenaWars:8 ,ArenaChance:8 ,ArenaBox:8 ,ArenaBox1:8, ArenaBox2:8
			,ArenaRev:8, ArenaPrize:8, LuckStar:32
			,LuckDia:32, LuckUsed:32, ValSum:32>> = Bin1,
            Rs#role{
                power = Power
                ,sign_days = SignDays
                ,sign_time = SignTime
                ,sign_old_days = SignOldDays
                ,growth = Growth
            	,bag_mat_max  = BagMatMax
				,bag_prop_max = BagPropMax
				,bag_equ_max  = BagEquMax
				,arena_time		= ArenaTime
				,arena_honor	= ArenaHonor
				,arena_wars		= ArenaWars
				,arena_chance	= ArenaChance
				,arena_rank_box = ArenaBox
				,arena_combat_box1 = ArenaBox1
				,arena_combat_box2 = ArenaBox2
				,arena_revenge	   = ArenaRev
				,arena_prize	   = ArenaPrize
				,luck = {LuckStar, LuckDia, LuckUsed, ValSum}
			}
    end.
%%.

%%% vim: set foldmethod=marker foldmarker=%%',%%.:
