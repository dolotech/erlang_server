%%----------------------------------------------------
%% $Id: mod_combat.erl 5772 2013-12-07 09:34:35Z rolong $
%%
%% 战斗模块
%%----------------------------------------------------

-module(mod_combat).
-export([
        combat/3
        ,arena/2
        ,init_monster/1
        %% ,fix_pos/1
    ]
).

-include("common.hrl").
-include("hero.hrl").
-include("fb.hrl").

arena(Rs, ArenaId) ->
    case Rs#role.produce_pass of
        {3, Type} ->
            case mod_arena:get_data(ArenaId) of
                {Robot, Rid, Lev, Name, S} ->
                    Heroes2 = case Robot of
                        1 ->
                            ?DEBUG("Robot PVP ****************** **", []),
                            init_monster1(S, []);
                        0 ->
                            ?DEBUG("Role PVP ****************** **", []),
                            mod_arena:fix_arena_pos(S)
                    end,
                    Heroes1 = mod_hero:get_combat_heroes(Rs#role.heroes, Rs#role.items),
                    HeroesTmp = [XX || XX <- Rs#role.heroes, XX#hero.pos > 0],
                    mod_battle:print_hero(HeroesTmp),
                    {Over, Data2} = mod_battle:battle(Heroes1, Heroes2),
                    {IsWin, AddExp, Gold, Report} = case Over =:= 1 of
                        true ->
                            E = (Lev - Rs#role.arena_lev) + 10,
                            G = case Type of
                                3 ->
                                    %% 成功揭榜
                                    gen_server:cast(arena, {del_offer_report, ArenaId}),
                                    %% 悬赏奖励
                                    Lev * 300;
                                _ ->
                                    %% 非揭榜战斗不给予金币奖励
                                    0
                            end,
                            case Robot of
                                0 ->
                                    %% 自己胜，对方败，给对方发送反击战报
                                    myevent:send_event(Rid, {lost_report, Rs#role.arena_id, Rs#role.name});
                                _ -> ok
                            end,
                            {1, E, G, undefined};
                        false ->
                            E = (Lev - Rs#role.arena_lev) - 10,
                            {0, E, 0, {ArenaId, Name}}
                    end,
                    {Lev1, Exp1, Rs1} = mod_arena:add_arena_exp(AddExp, Rs),
                    Rs2 = lib_role:add_attr(gold, Gold, Rs1),
                    Produce = {3, [Gold, AddExp, Lev1, Exp1], Report},
                    Rs3 = Rs2#role{produce_pass = Produce},
                    mod_arena:set_data(Rs, Lev1, Exp1, Heroes1),
                    Rs4 = case IsWin of
                        1 ->
                            mod_arena:update_arena_state(Name, Rs3);
                        0 ->
                            Rs3
                    end,
                    %% 竞技场成就推送
                    Rs6 = mod_attain:attain_state(36, 1, Rs4),
                    Rs5 = case IsWin of
                        1 ->
                            mod_attain:attain_state(37, 1, Rs6);
                        0 ->
                            Rs6
                    end,
                    #role{arena_wars = WarsNum, arena_prize = PrizeNum} = Rs5,
                    Rs0 = case Type of
                        3 ->
                            Rs5#role{arena_wars = WarsNum - 1, arena_prize = PrizeNum + 1};
                        _ ->
                            Rs5#role{arena_wars = WarsNum - 1}
                    end,
                    {[Rs0#role.power, 0, IsWin, Data2, []], Rs0};
                {error, Reason} ->
                    ?WARN("ARENA COMBAT ERROR: ~w", [Reason]),
                    {[0, 0, 0, [], []], Rs}
            end;
        E ->
            ?ERR("ERROR:~w", [E]),
            {[0, 0, 0, [], []], Rs}
    end.


combat(1, Rs, ReqPassId) ->
    #role{items = Items, tollgate_id = PassId,
          power = Power} = Rs,
    ?DEBUG("Main CurGateId:~w, ReqGateId:~w", [PassId, ReqPassId]),
    Data = data_tollgate:get({1, ReqPassId}),
    if
        Data == undefined ->
            ?WARN("Error ReqPassId: ~w", [{1, ReqPassId}]),
            {[0, 0, 0, [], []], Rs};
        ReqPassId > PassId ->
            ?WARN("Error ReqPassId: ~w, PassId:~w", [ReqPassId, PassId]),
            {[0, 0, 0, [], []], Rs};
        true ->
            Power1 = Power - util:get_val(power, Data),
            case Power1 > 0 of
                true ->
                    Monsters = util:get_val(monsters, Data),
                    SumExp  = util:get_val(exp, Data),
                    Heroes2 = init_monster1(Monsters, []),
                    Heroes1 = mod_hero:get_combat_heroes1(Rs#role.heroes, Items),
                    HeroesTmp = [XX || XX <- Rs#role.heroes, XX#hero.pos > 0],
                    mod_battle:print_hero(HeroesTmp),
                    %% Heroes11 = fix_pos(Heroes1),
                    {Over, Data2} = mod_battle:battle(Heroes1, Heroes2),
                    {Power2, IsWin, Rs2, Data3} =
                    case Over =:= 1 of
                        true ->
                            {NewHeroes, HeroesInfo} = add_heroes_exp(Heroes1, SumExp, Rs#role.heroes),
                            NewGateId = case (ReqPassId + 1) > PassId of
                                            true -> ReqPassId + 1;
                                            false -> PassId
                                        end,
                            Rs1 = Rs#role{
                                    heroes = NewHeroes,
                                    produce_pass = {1, ReqPassId},
                                    tollgate_id = NewGateId,
                                    power = Power1
                                   },
							%% 关卡成就推送
							A = util:floor(PassId / 50),
							B = util:floor(NewGateId / 50),
							Rs3 = case A < B of
									  true ->
										  mod_attain:attain_state(8, 1, Rs1);
									  false ->
										  Rs1
								  end,
							Rs4 = mod_attain:attain_state(35, 1, Rs3),
                            {Power1, 1, Rs4, HeroesInfo};
                        false ->
                            {Power, 0, Rs, []}
                    end,
					%% 主线战斗成就推送
					Rs0 = mod_attain:attain_state(34, 1, Rs2),
                    {[Power2, PassId, IsWin, Data2, Data3], Rs0};
                false ->
                    ?WARN("Power: ~w -> ~w", [Power, Power1]),
                    {[0, 0, 0, [], []], Rs}
            end
    end;

combat(2, Rs, ReqPassId) ->
    #role{items = Items} = Rs,
    ?DEBUG("Fb GateId:~w", [ReqPassId]),
    Data = data_tollgate:get({2, ReqPassId}),
    if
        Data == undefined ->
            ?WARN("Error ReqPassId: ~w", [{2, ReqPassId}]),
            {[0, 0, 0, [], []], Rs};
        true ->
            Monsters = util:get_val(monsters, Data),
            Heroes2 = init_monster1(Monsters, []),
            Heroes1 = mod_hero:get_combat_heroes(Rs#role.heroes, Items),
            HeroesTmp = [XX || XX <- Rs#role.heroes, XX#hero.pos > 0],
            mod_battle:print_hero(HeroesTmp),
            {Over, Data2} = mod_battle:battle(Heroes1, Heroes2),
            {IsWin, Rs2} =
            case Over =:= 1 of
                true ->
					Rs1 = mod_attain:combat_attain(ReqPassId, Rs),
					{1, fix_fb_combat(Rs1, ReqPassId)};
                false -> {0, Rs}
                % false -> {1, fix_fb_combat(Rs, ReqPassId)}
            end,
            {[Rs#role.power, ReqPassId, IsWin, Data2, []], Rs2}
    end.

fix_fb_combat(Rs, ReqPassId) ->
    Rs1 = case Rs#role.fb_gate of
        {1, _} -> Rs#role{ fb_combat1 = Rs#role.fb_combat1 - 1 };
        {2, _} -> Rs#role{ fb_combat2 = Rs#role.fb_combat2 - 1 };
        {3, _} -> Rs#role{ fb_combat3 = Rs#role.fb_combat3 - 1 };
        Else -> ?ERR("Error Gate: ~w", [Else]), Rs
    end,
    Rs1#role{
      fb_gate = undefined
      ,produce_pass = {2, ReqPassId}
     }.

init_monster(Data) ->
    init_monster1(Data, []).

init_monster1([], Result) -> Result;
init_monster1([{Tid, Pos} | Monsters], Result) ->
    Result1 = case data_monster:get(Tid) of
        undefined ->
            ?WARN("Error monster id: ~w", [Tid]),
            Result;
        Data ->
            Pos1     = 20 + Pos,
            Hp       = util:get_val(hp    , Data),
            Atk      = util:get_val(atk   , Data),
            Def      = util:get_val(def       , Data),
            Pun      = util:get_val(pun       , Data),
            Hit      = util:get_val(hit       , Data),
            Dod      = util:get_val(dod       , Data),
            Crit = util:get_val(crit , Data),
            CritNum  = util:get_val(crit_num  , Data),
            CritAnit = util:get_val(crit_anti , Data),
            Tou      = util:get_val(tou       , Data, 0),
            TouAnit  = util:get_val(tou_anit  , Data, 0),
            Hero = #hero{
                id         = 0
                ,pos       = Pos1
                ,tid       = Tid
                ,hp        = Hp
                ,atk       = Atk
                ,def       = Def
                ,pun       = Pun
                ,hit       = Hit
                ,dod       = Dod
                ,crit = Crit
                ,crit_num  = CritNum
                ,crit_anti = CritAnit
                ,tou       = Tou
                ,tou_anit  = TouAnit
                ,skills = lib_role:init_skills(Data)
            },
            [Hero | Result]
    end,
    init_monster1(Monsters, Result1).
%%.

%%' fix_pos
%% fix_pos(Heroes) ->
%%     Poses = util:shuffle([11, 12, 13, 14, 15, 16, 17, 18, 19]),
%%     fix_pos1(Heroes, Poses, []).
%%
%% fix_pos1([H = #hero{pos = Pos} | Heroes], [PosH | PosT], Rt) ->
%%     Pos1 = if
%%         Pos >= 11, Pos =< 19 ->
%%             case lists:member(Pos, [PosH | PosT]) of
%%                 true -> ok;
%%                 false -> ?WARN("Repeat Pos: ~w", [Pos])
%%             end,
%%             Pos;
%%         true -> PosH
%%     end,
%%     PosL = lists:delete(Pos1, [PosH | PosT]),
%%     Rt1 = [H#hero{pos = Pos1} | Rt],
%%     fix_pos1(Heroes, PosL, Rt1);
%% fix_pos1([], _, Rt) ->
%%     Rt.
%%.

%%' add_heroes_exp
add_heroes_exp(_CombatHeroes, 0, Heroes) ->
    {Heroes, []};
add_heroes_exp(CombatHeroes, SumExp, Heroes) ->
    add_heroes_exp1(CombatHeroes, SumExp, Heroes, []).

add_heroes_exp1([], _Exp, Heroes, Rt) -> {Heroes, Rt};
add_heroes_exp1([#hero{id = Id} | T], Exp, Heroes, Rt) ->
    Hero = mod_hero:get_hero(Id, Heroes),
    Hero1 = mod_hero:add_hero_exp(Hero, Exp),
    #hero{pos = Pos, lev = Lev, exp = Exp1} = Hero1,
    Rt1 = [[Pos, Lev, Exp1] | Rt],
    Heroes1 = mod_hero:set_hero(Hero1, Heroes),
    add_heroes_exp1(T, Exp, Heroes1, Rt1).
%%.

%%' zip / unzip
%% unzip([Val]) ->
%%     <<Version:8, Bin1/binary>> = Val,
%%     case Version of
%%         1 ->
%%             <<?HERO_ZIP_VERSION:8
%%             ,Id       :32
%%             ,MapId    :8
%%             ,MapPos1  :32
%%             ,MapPos2  :32
%%             ,MyPos1   :32
%%             ,MyPos2   :32
%%             ,RTime    :32
%%             ,FreeTimes:32
%%             ,Bin2/binary
%%             >> = Bin1,
%%             Rule = [[int32, int8, int16, int8, int32]],
%%             Data = protocol_fun:unpack(Rule, Bin1),
%%             %% ?INFO("unzip step:~w, skills:~w", [Step, Skills]),
%%             Data = data_hero:get(Tid),
%%             #hero{
%%                 id         = Id
%%                 ,tid       = Tid
%%                 ,job       = Job
%%                 ,sort      = Sort
%%                 ,frequency = Frequency
%%                 ,hp        = Hp
%%                 ,atk       = Atk
%%                 ,def       = Def
%%                 ,pun       = Pun
%%                 ,hit       = Hit
%%                 ,dod       = Dod
%%                 ,crit = Crit
%%                 ,crit_num  = CritNum
%%                 ,crit_anti = CritAnit
%%                 ,tou       = Tou
%%                 ,pos       = Pos
%%                 ,exp_max   = ExpMax
%%                 ,exp       = Exp
%%                 ,lev       = Lev
%%                 ,step      = Step
%%                 ,quality   = Quality
%%                 ,skills = lib_role:init_skills(Data)
%%             };
%%         _ ->
%%             ?ERR("undefined version: ~w", [Version]),
%%             undefined
%%     end.

%% -define(HERO_ZIP_VERSION, 1).

%% zip(Fb) ->
%%     #fb{
%%           id            = Id
%%           ,map_id       = MapId
%%           ,map_pos1     = MapPos1
%%           ,map_pos2     = MapPos2
%%           ,mypos1       = MyPos1
%%           ,mypos2       = MyPos2
%%           ,refresh_time = RTime
%%           ,free_times   = FreeTimes
%%           ,monsters     = Ms
%%     } = Fb,
%%     MsLen = length(Ms),
%%     MsBin = list_to_binary([<<X1:32, X2:8, X3:16, X4:8, X5:32>> ||
%%             #fbm{
%%                 pos           = X1
%%                 ,type         = X2
%%                 ,gate_id      = X3
%%                 ,status       = X4
%%                 ,revival_time = X5
%%             } <- Ms]),
%%     <<?HERO_ZIP_VERSION:8
%%     ,Id       :32
%%     ,MapId    :8
%%     ,MapPos1  :32
%%     ,MapPos2  :32
%%     ,MyPos1   :32
%%     ,MyPos2   :32
%%     ,RTime    :32
%%     ,FreeTimes:32
%%     ,MsLen    :32
%%     ,MsBin/binary
%%     >>.
%%.
%% vim: set filetype=erlang foldmarker=%%',%%. foldmethod=marker:
