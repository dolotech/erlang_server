%%----------------------------------------------------
%% 协议14 - hero
%% $Id: pt_hero.erl 5778 2013-12-07 10:06:54Z rolong $
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-module(pt_hero).
-export([handle/3]).

-include("common.hrl").
-include("hero.hrl").
-include("equ.hrl").

-define(SEARCHED_HERO, searched_hero).

%% 布阵
handle(14001, [L], Rs) ->
    case mod_hero:set_pos(Rs, L) of
        {ok, Rs1} -> {ok, [0], Rs1};
        {error, _} -> {ok, [1]}
    end;

%% 英雄列表
handle(14002, [], Rs) ->
    Data = [mod_hero:pack_hero(X) ||
        X <- Rs#role.heroes],
    % ?DEBUG("Hero:~w", [Data]),
    Rs1 = mod_attain:hero_attain(10, 15, Rs),
    Rs2 = mod_attain:hero_attain(11, 30, Rs1),
    Rs3 = mod_attain:hero_attain(12, 45, Rs2),
    Rs0 = mod_attain:hero_attain(13, 60, Rs3),
    {ok, [Data], Rs0};

%% handle(14005, [ItemId, HeroId, Pos], Rs) ->
%%     %% ?DEBUG("ItemId:~w, HeroId:~w, Del:~w", [ItemId, HeroId, Del]),
%%     Items = Rs#role.items,
%%     Heroes = Rs#role.heroes,
%%     Hero = mod_hero:get_hero(HeroId, Heroes),
%%     Item = mod_item:get_item(ItemId, Items),
%%     % ?DEBUG("===Items:~w, Heroes:~w, Hero:~w, Item:~w ", [Items,Heroes,Hero,Item]),
%%     if
%%         Hero == false ->
%%             {ok, [127]};
%%         Item == false ->
%%             {ok, [128]};
%%         true ->
%%             Data = data_item:get(Item#item.tid),
%%             SkillId = util:get_val(ctl1, Data, 0),
%%             case mod_hero:set_skill(Pos, SkillId, Hero) of
%%                 {ok, Hero1} ->
%%                     Heroes1 = mod_hero:set_hero(Hero1, Heroes),
%%                     Rs1 = Rs#role{heroes = Heroes1},
%%                     {ok, [0], Rs1};
%%                 {error, Reason} ->
%%                     ?WARN("set_skill:~w", [Reason]),
%%                     {ok, [129]}
%%             end
%%     end;

%% 净化
%% -> [Code, EH, Setp, ER]
handle(14010, [HeroId], Rs) ->
    #role{heroes = Heroes, items = Items} = Rs,
    % ?DEBUG("===Heroes:~w, Items:~w", [hd(Heroes), hd(Items)]),
    Hero = mod_hero:get_hero(HeroId, Heroes),
    if
        Hero == false ->
            {ok, [127]};
        true ->
            case data_purge:get(Hero#hero.quality) of
                undefined ->
                    {ok, [4]};
                Data ->
                    Materials = util:get_val(materials, Data),
                    % {materials, [{13002,20},{13001,5},{13003,1},{13004,1}]}
                    Type = util:get_val(type, Data),
                    Num = util:get_val(num, Data),
                    {CType, RtCode} = case Type of
                        1 -> {gold, 1};
                        2 -> {diamond, 2}
                    end,
                    case lib_role:spend(CType, Num, Rs) of
                        {ok, Rs1} ->
                            case mod_item:del_items(by_tid, Materials, Items) of
                                {ok, Items1, Dels} ->
                                    Hero1 = mod_hero:upgrade_quality(Hero),
                                    mod_hero:hero_notice(Hero1),
                                    Heroes1 = mod_hero:set_hero(Hero1, Heroes),
                                    Rs2 = Rs1#role{items = Items1, heroes = Heroes1, save = [items, role, heroes]},
                                    lib_role:notice(Rs2),
                                    mod_item:send_notice(Rs#role.pid_sender, Dels),
                                    %% 成就推送
                                    Rs3 = case Hero1#hero.quality >= 5 of
                                        true -> mod_attain:attain_state(49, 1, Rs2);
                                        false -> Rs2
                                    end,
                                    Rs4 = mod_attain:attain_state(45, 1, Rs3),
                                    Rs0 = lib_role:spend_ok(CType, 30, Rs, Rs4),
                                    {ok, [0], Rs0};
                                {error, _} ->
                                    {ok, [3]}
                            end;
                        {error, _} ->
                            {ok, [RtCode]}
                    end
            end
    end;

%% delete hero
%% -> [Code, HeroId, Essence]
handle(14015, [HeroId], Rs) ->
    case mod_hero:del_hero(Rs, HeroId) of
        {ok, Rs1} ->
            {ok, [0, HeroId, Rs1#role.essence], Rs1};
        {error, Reason} ->
            ?DEBUG("14015 ERROR:~w", [Reason]),
            {ok, [128, 0, 0]}
    end;

%% 英雄锁
handle(14019, [Id, Lock], Rs) ->
    ?DEBUG("lock, Id:~w, Lock:~w", [Id, Lock]),
    Tavern = Rs#role.tavern,
    case lists:keyfind(Id, 1, Tavern) of
        false ->
            ?DEBUG("When lock, Id:~w, Lock:~w, not found", [Id, Lock]),
            {ok, [127]};
        {_, _, Hero} ->
            Tavern1 = lists:keyreplace(Id, 1, Tavern, {Id, Lock, Hero}),
            Rs1 = Rs#role{tavern = Tavern1},
            {ok, [0], Rs1}
    end;

%%----------------------------------------------------
%% 请求酒馆数据，
%% 如果是注册后第一次进酒馆，
%% 会自动刷新一次
%%----------------------------------------------------
handle(14020, [0], Rs) ->
    case mod_hero:db_init_tavern(Rs) of
        {ok, Rs1} ->
            RestTime = mod_hero:tavern_rest_time(Rs1),
            HeroesData = pack_tavern(Rs#role.tavern, []),
            {ok, [0, RestTime, HeroesData], Rs1};
        {error, Reason} ->
            ?WARN("Error When Init Tavern: ~w", [Reason]),
            {ok, [127, 0, []]}
    end;

%%----------------------------------------------------
%% 酒馆刷新(最多3个英雄)
%%----------------------------------------------------
handle(14020, [1], Rs) when Rs#role.tavern_time =/= undefined ->
    RestTime = mod_hero:tavern_rest_time(Rs),
    SearchPrice1 = case RestTime > 0 of
        true ->
            %% 按时间计算扣费额
            Unit = data_config:get(diamond_per_min),
            util:ceil(RestTime / 60 * Unit);
        false -> 0
    end,
    ?DEBUG("=== Tavern SearchPrice:~w, RestTime:~w ===", [SearchPrice1, RestTime]),
    case lib_role:spend(diamond, SearchPrice1, Rs) of
        {ok, Rs1} ->
            Tavern1 = mod_hero:tavern_refresh(Rs1#role.tavern),
            HeroesData = pack_tavern(Tavern1, []),
            Max = data_config:get(refresh_time),
            Rs2 = Rs1#role{
                save = [role, tavern]
                ,tavern = Tavern1
                ,tavern_time = util:unixtime()
            },
            lib_role:notice(diamond, Rs2),
            Rs3 = lib_role:spend_ok(diamond, 18, Rs, Rs2),
            Rs0 = mod_attain:attain_state(32, 1, Rs3),
            {ok, [0, Max, HeroesData], Rs0};
        {error, _} ->
            {ok, [2, 0, []]}
    end;

handle(14020, [1], _Rs) ->
    ?WARN("Tavern not initialized!", []),
    {128, 0, []};

%% 酒馆中购买英雄
%% # 消息代码：
%% # Code:
%% # 0 = 成功
%% # 1 = 金币不足
%% # 2 = 钻石不足
%% # >=127 = 程序异常
%% 'code' => 'int32',
handle(14022, [Id], Rs) ->
    Tavern = Rs#role.tavern,
    case lists:keyfind(Id, 1, Tavern) of
        false ->
            ?DEBUG("When Buy, Id:~w, Tavern:~w", [Id, Tavern]),
            {ok, [127]};
        {_, _, Hero} ->
            #hero{rare = Rare, quality = Q} = Hero,
            case data_hero_price:get({Rare, Q}) of
                undefined -> {ok, [128]};
                [{type, Type}, {price, Price}] ->
                    {CType, RtCode} = case Type of
                        1 -> {gold, 1};
                        2 -> {diamond, 2}
                    end,
                    case lib_role:spend(CType, Price, Rs) of
                        {ok, Rs1} ->
                            Tavern1 = lists:keydelete(Id, 1, Tavern),
                            Rs2 = Rs1#role{tavern = Tavern1},
                            {ok, HeroId, Rs3} = lib_role:get_new_id(hero, Rs2),
                            Hero1 = Hero#hero{id = HeroId},
                            Heroes = [Hero1 | Rs3#role.heroes],
                            Rs4 = Rs3#role{tavern = Tavern1, heroes = Heroes, save = [role, heroes]},
                            mod_hero:hero_notice(Rs4#role.pid_sender, Hero1),
                            lib_role:notice(Rs4),
                            Rs5 = lib_role:spend_ok(CType, 7, Rs, Rs4),
                            %% 成就推送
                            Rs0 = case Hero1#hero.quality >= 5 of
                                true -> mod_attain:attain_state(49, 1, Rs5);
                                false -> Rs5
                            end,
                            mod_hero:db_update_tavern(Rs0),
                            {ok, [0], Rs0};
                        {error, _} ->
                            {ok, [RtCode]}
                    end
            end
    end;

%% 赠送英雄
%% TODO: 1 = 已经赠送(限制赠送)
%% # 消息代码：
%% # Code:
%% # 0 = 成功
%% # 1 = 已经赠送
%% # >=127 = 程序异常
%% 'code' => 'int8',
handle(14023, [Id], Rs) ->
    ?DEBUG("14023:~w, Growth:~w", [Id, Rs#role.growth]),
    case Rs#role.growth =< 1 of
        true ->
            %% L = [30002,30003,30004,30005],
			L = data_hero_give:get(ids),
            case lists:member(Id, L) of
                false ->
                    ?DEBUG("14023 Not member", []),
                    {ok, [127]};
                true  ->
					%% NewId:英雄唯一Id
					%% {ok, NewId, Rs1} = lib_role:get_new_id(hero, Rs),
					%% Hero = mod_hero:give_hero(NewId, Id),
					%% mod_hero:db_insert(Rs1#role.id, Hero),
					%% Rs2 = Rs1#role{heroes = [Hero | Rs#role.heroes]},
					{ok, Rs1, Hero} = mod_hero:add_give_hero(Rs, Id),
                    mod_hero:hero_notice(Rs1#role.pid_sender, Hero),
                    Rs2 = Rs1#role{growth = 2},
                    {ok, [0], Rs2}
            end;
        false ->
            ?DEBUG("14023 No login", []),
            {ok, [128]}
    end;

%% 英雄吞噬
%% 吞噬消耗的金币计算：
%% FLOOR【吞噬的总经验/50】+300
%% 吞噬转移的经验计算：
%% FLOOR【所有被吞噬的英雄经验累加*0.8】+500
handle(14030, [HeroId, Id1, Id2, Id3, Id4], Rs) ->
    #role{id = Rid, heroes = Heroes, items = Items} = Rs,
    case mod_hero:get_hero(HeroId, Heroes) of
        false ->
            {ok, [127, 0, 0, 0]};
        Hero ->
            L = [{Id1, 1}, {Id2, 1}, {Id3, 1}, {Id4, 1}],
            case absorb(L, Rid, Heroes, Items, [], 0) of
                {ok, Heroes1, Items1, Cmd, AddExp} ->
                    SpendGold = util:floor(AddExp/50+300),
                    case lib_role:spend(gold, SpendGold, Rs) of
                        {ok, Rs1} ->
                            AddExp1 = util:floor(AddExp * 0.8 + 500),
                            ?DEBUG("AddExp:~w", [AddExp1]),
                            ?DEBUG("HERO1 LEV:~w EXP:~w", [Hero#hero.lev, Hero#hero.exp]),
                            Hero1 = mod_hero:add_hero_exp(Hero, AddExp1),
                            ?DEBUG("HERO2 LEV:~w EXP:~w", [Hero1#hero.lev, Hero1#hero.exp]),
                            Heroes2 = mod_hero:set_hero(Hero1, Heroes1),
                            L2 = [X1 || {X1, _X2} <- L, X1 > 0],
                            Items2 = fix_items(Items1, L2),
                            Rs2 = Rs1#role{heroes = Heroes2, items = Items2, save = [items]},
                            lib_role:exec_cmd(Cmd),
                            lib_role:notice(Rs2),
                            DelL = [X || {X, _} <- L, X > 0],
                            sender:pack_send(Rs#role.pid_sender, 14015, [DelL]),
                            %% 成就推送
                            Rs3 = mod_attain:attain_state(44, 1, Rs2),
                            Rs0 = lib_role:spend_ok(gold, 5, Rs, Rs3),
                            %% ?DEBUG("herolev:~w", [Hero1#hero.lev]),
                            {ok, [0, HeroId, Hero1#hero.lev, Hero1#hero.exp], Rs0#role{save = [role, heroes]}};
                        {error, _} ->
                            {ok, [1, 0, 0, 0]}
                    end;
                {error, Reason} ->
                    ?WARN("~w", [Reason]),
                    {ok, [128, 0, 0, 0]}
            end
    end;

handle(_Cmd, _Data, _Rs) ->
    {error, bad_request}.

%% === 私有函数 ===

pack_tavern([{_Id, _IsLock, false} | T], Data) ->
    pack_tavern(T, Data);
pack_tavern([{Id, IsLock, Hero} | T], Data) ->
    #hero{
        tid = Tid
        ,rare = Rare
        ,quality = Qua
        %% ,hp        = Hp
        %% ,atk       = Atk
        %% ,def       = Def
        %% ,pun       = Pun
        %% ,hit       = Hit
        %% ,dod       = Dod
        %% ,crit = Crit
        %% ,crit_num  = CritNum
        %% ,crit_anti = CritAnit
        %% ,tou       = Tou
    } = Hero,
    Rt = [Id, Tid, IsLock, Qua, Rare
        %% ,Hp
        %% ,Atk
        %% ,Def
        %% ,Pun
        %% ,Hit
        %% ,Dod
        %% ,Crit
        %% ,CritNum
        %% ,CritAnit
        %% ,Tou
    ],
    Data1 = [Rt | Data],
    pack_tavern(T, Data1);
pack_tavern([], Data) ->
    Data.

absorb([{0, _} | T], Rid, Heroes, Items, Cmd, AddExp) ->
    absorb(T, Rid, Heroes, Items, Cmd, AddExp);

absorb([{HeroId, 1} | T], Rid, Heroes, Items, Cmd, AddExp) ->
    case mod_hero:get_hero(HeroId, Heroes) of
        false -> {error, error_hid};
        #hero{exp = Exp, lev = Lev} ->
            Data = data_exp:get(Lev),
            ExpSum = util:get_val(exp_sum, Data, 0),
            AddExp1 = AddExp + Exp + ExpSum,
            ?DEBUG("ABSORB Lev:~w, AccExp:~w+Exp:~w+ExpSum:~w=~w", [Lev, AddExp, Exp, ExpSum, AddExp1]),
            Heroes1 = lists:keydelete(HeroId, 2, Heroes),
            Cmd1 = [{mod_hero, db_delete, [Rid, HeroId]} | Cmd],
            absorb(T, Rid, Heroes1, Items, Cmd1, AddExp1)
    end;
absorb([{ItemId, 2} | T], Rid, Heroes, Items, Cmd, AddExp) ->
    case mod_hero:get_item(ItemId, Heroes) of
        false -> {error, error_item_id};
        #item{sort = 3, tid = Tid} ->
            Data = data_prop:get(Tid),
            Exp = util:get_val(control1, Data, 0),
            AddExp1 = AddExp + Exp,
            case mod_item:del_item(by_id, ItemId, 1, Items) of
                {ok, Items1, Dels} ->
                    Cmd1 = [{mod_item, db_delete, [Rid, ItemId]} | Cmd],
                    Cmd2 = [{mod_item, send_notice, [Rid, Dels]} | Cmd1],
                    absorb(T, Rid, Heroes, Items1, Cmd2, AddExp1);
                {error, Reason} ->
                    {error, Reason}
            end;
        _ -> {error, error_tid}
    end;
absorb([], _, Heroes, Items, Cmd, AddExp) ->
    {ok, Heroes, Items, Cmd, AddExp}.

fix_items(Items, L) ->
    fix_items(Items, L, []).

fix_items([], _L, Rt) ->
    Rt;
fix_items([Item | Items], L, Rt) ->
    case Item#item.tid >= ?MIN_EQU_ID andalso
        lists:member(Item#item.attr#equ.hero_id, L) of
        true ->
            ?DEBUG("fix_items: hero_id:~w", [Item#item.attr#equ.hero_id]),
            Attr = Item#item.attr#equ{hero_id = 0},
            Item1 = Item#item{attr = Attr, changed = 1},
            fix_items(Items, L, [Item1 | Rt]);
        false ->
            fix_items(Items, L, [Item | Rt])
    end.


%% vim: set ft=erlang foldmethod=marker foldmarker=%%',%%.:
