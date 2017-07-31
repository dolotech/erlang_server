%%----------------------------------------------------
%% $Id: mod_hero.erl 5808 2013-12-09 03:33:12Z rolong $
%%
%% Hero
%%----------------------------------------------------

-module(mod_hero).
-export([
        add_hero/2
		,add_give_hero/2
        ,get_hero/2
        ,set_hero/2
        ,set_pos/2
        ,get_combat_heroes/2
        ,get_combat_heroes1/2
        ,db_init/1
        ,db_update/2
        ,db_insert/2
        ,db_delete/2
        %% ,set_skill/3
        ,add_hero_exp/3
        ,add_hero_exp/2
        ,del_hero/2
        ,init_hero/2
        ,init_hero/1
		,give_hero/2
        ,hero_notice/2
        ,upgrade_quality/1
        ,pack_hero/1
        ,tavern_rest_time/1
        ,zip/1
        ,unzip/1
        ,calc_power/1
        ,calc_power/2
        ,tavern_refresh/1
        ,db_init_tavern/1
        ,db_update_tavern/1
    ]).

-include("common.hrl").
-include("hero.hrl").
-include("equ.hrl").


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(TEST).
data_hero_test() ->
    ?assert(undefined == data_hero:get(0)),
    Ids = data_hero:get(ids),
    ?assert(length(Ids) > 0),
    lists:foreach(fun(Id) ->
                Data = data_hero:get(Id),
                ?assert(util:get_val(job, Data, 0) > 0),
                ?assert(util:get_val(sort, Data, 0) > 0)
        end, Ids),
    ok.
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
%%' 战斗力=INT((暴击+闪避+命中+抗暴+防御+穿刺+韧性*2)/1000*（血量/7+攻击）)

-spec calc_power(Heroes :: [#hero{}]) ->
    Power :: integer().

calc_power(Heroes) ->
    calc_power(Heroes, 0).

calc_power([H | T], Power) ->
    Power1 = calc_power1(H) + Power,
    calc_power(T, Power1);
calc_power([], Power) ->
    Power.

calc_power1(Hero) ->
    #hero{
        hp        = Hp
        ,atk       = Atk
        ,def       = Def
        ,pun       = Pun
        ,hit       = Hit
        ,dod       = Dod
        ,crit = Crit
        ,crit_num  = CritNum
        ,crit_anti = CritAnit
        ,tou       = Tou
    } = Hero,
    util:ceil((Crit + Dod + Hit + CritAnit + CritNum + Def + Pun + Tou * 2) / 1000 * (Hp / 7 + Atk)).
%%.

%%' 增加英雄
-spec add_hero(Rs, Tid) -> {ok, NewRs, NewHero} when
    Rs :: #role{},
    NewRs :: #role{},
    NewHero :: #hero{},
    Tid :: integer().

add_hero(Rs, Tid) ->
    {ok, Id, Rs1} = lib_role:get_new_id(hero, Rs),
    Hero = init_hero(Id, Tid),
    db_insert(Rs#role.id, Hero),
    {ok, Rs1#role{heroes = [Hero | Rs#role.heroes]}, Hero}.

%% 增加赠送英雄
add_give_hero(Rs, Tid) ->
    {ok, Id, Rs1} = lib_role:get_new_id(hero, Rs),
    Hero = give_hero(Id, Tid),
    db_insert(Rs1#role.id, Hero),
    {ok, Rs1#role{heroes = [Hero | Rs#role.heroes]}, Hero}.
%%.

%% 计算酒馆刷新剩余秒数
tavern_rest_time(Rs) ->
    #role{tavern_time = TavernTime} = Rs,
    case is_integer(TavernTime) andalso TavernTime > 0 of
        true ->
            CurTime = util:unixtime(),
            Max = data_config:get(refresh_time),
            max(0, Max - (CurTime - TavernTime));
        false -> 0
    end.

pack_hero(Hero) ->
    #hero{
        id  = Id
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
        ,pos       = Pos
        ,exp       = Exp
        ,lev       = Lev
        ,quality   = Quality
    } = Hero,
    [
        Id
        ,Tid
        ,Pos
        ,Quality
        ,Lev
        ,Exp
        % --- Base ---
        ,Hp
        ,Atk
        ,Def
        ,Pun
        ,Hit
        ,Dod
        ,Crit
        ,CritNum
        ,CritAnit
        ,Tou
    ].

hero_notice(Sender, Hero) ->
    Data = pack_hero(Hero),
    sender:pack_send(Sender, 14025, Data).

%% quality2string(1) ->   d;
%% quality2string(2) ->   c;
%% quality2string(3) ->   b;
%% quality2string(4) ->   a;
%% quality2string(5) ->   s;
%% quality2string(6) ->  ss;
%% quality2string(7) -> sss;
%% quality2string(_) -> undefined.

%% add_essence(Hero, AddEssence) ->
%%     #hero{tid = Tid, essence = Essence, hp = Hp,
%%         atk = Atk, step = Step, skills = Skills} = Hero,
%%     Essence1 = Essence + AddEssence,
%%     case data_purge:get({Tid, Step + 1}) of
%%         [{EssenceMax, SkillPos, _ToE, HpMaxP, AtkP}] ->
%%             case Essence1 >= EssenceMax of
%%                 true ->
%%                     Essence2 = Essence1 - EssenceMax,
%%                     HpMax1 = util:ceil(Hp * (HpMaxP / 100 + 1)),
%%                     Atk1 = util:ceil(Atk * (AtkP / 100 + 1)),
%%                     %% 开放技能
%%                     Skills1 = case SkillPos > 0 of
%%                         true ->
%%                             case lists:keymember(SkillPos, 1, Skills) of
%%                                 true ->
%%                                     ?WARN("Repeat unlock skill, Pos:~w, Skills:~w", [SkillPos, Skills]),
%%                                     Skills;
%%                                 false ->
%%                                     [{SkillPos, 0, 0, 0} | Skills]
%%                             end;
%%                         false ->
%%                             Skills
%%                     end,
%%                     {ok, Hero#hero{essence = Essence2, skills = Skills1,
%%                             hp = HpMax1, atk = Atk1, step = Step+1,
%%                             changed = 1}};
%%                 false ->
%%                     {ok, Hero#hero{essence = Essence1}}
%%             end;
%%         undefined ->
%%             {error, top};
%%         _Data ->
%%             ?WARN("undefined purge data: ~w", [_Data]),
%%             {error, top}
%%     end.

del_hero(Rs, HeroId) ->
    #role{id = Rid, heroes = Heroes, essence = Essence} = Rs,
    case get_hero(HeroId, Heroes) of
        false -> {error, no_hero};
        Hero ->
            #hero{tid = Tid, step = Step} = Hero,
            case data_purge:get({Tid, Step}) of
                [{_EssenceMax, _Skill, ToEssence, _HpMaxP, _AtkP}] ->
                    Essence1 = Essence + ToEssence,
                    Heroes1 = lists:keydelete(HeroId, 2, Heroes),
                    Rs1 = Rs#role{essence = Essence1, heroes = Heroes1},
                    case db_delete(Rid, HeroId) of
                        {ok, 1} -> {ok, Rs1};
                        {ok, 0} -> {error, no_hero};
                        {error, Reason} -> {error, Reason}
                    end;
                undefined ->
                    {error, no_data};
                _Data ->
                    ?WARN("undefined purge data: ~w", [_Data]),
                    {error, no_data}
            end
    end.

get_equs(Hero, [Item | Items]) ->
    #item{tid = Tid, attr = Equ} = Item,
    case Tid >= ?MIN_EQU_ID andalso Equ#equ.hero_id == Hero#hero.id of
        true ->
            Equ1 = set_embed_attr(Equ#equ.embed, Equ),
            #equ{
                hp        = Hp
                ,atk       = Atk
                ,def       = Def
                ,pun       = Pun
                ,hit       = Hit
                ,dod       = Dod
                ,crit      = Crit
                ,crit_num  = CritNum
                ,crit_anti = CritAnit
                ,tou       = Tou
            } = Equ1,
            Hero1 = Hero#hero{
                hp         = Hero#hero.hp        + Hp
                ,atk       = Hero#hero.atk       + Atk
                ,def       = Hero#hero.def       + Def
                ,pun       = Hero#hero.pun       + Pun
                ,hit       = Hero#hero.hit       + Hit
                ,dod       = Hero#hero.dod       + Dod
                ,crit      = Hero#hero.crit      + Crit
                ,crit_num  = Hero#hero.crit_num  + CritNum
                ,crit_anti = Hero#hero.crit_anti + CritAnit
                ,tou       = Hero#hero.tou       + Tou
            },
            %% ?INFO("Tid(~w) Base:~w", [Item#item.tid, Base]),
            get_equs(Hero1, Items);
        false -> get_equs(Hero, Items)
    end;
get_equs(Hero, []) -> Hero.

set_embed_attr([{_, ?hp      , V} | T], Equ) -> Equ1 = Equ#equ{hp       = Equ#equ.hp       + V}, set_embed_attr(T, Equ1);
set_embed_attr([{_, ?atk     , V} | T], Equ) -> Equ1 = Equ#equ{atk      = Equ#equ.atk      + V}, set_embed_attr(T, Equ1);
set_embed_attr([{_, ?def     , V} | T], Equ) -> Equ1 = Equ#equ{def      = Equ#equ.def      + V}, set_embed_attr(T, Equ1);
set_embed_attr([{_, ?pun     , V} | T], Equ) -> Equ1 = Equ#equ{pun      = Equ#equ.pun      + V}, set_embed_attr(T, Equ1);
set_embed_attr([{_, ?hit     , V} | T], Equ) -> Equ1 = Equ#equ{hit      = Equ#equ.hit      + V}, set_embed_attr(T, Equ1);
set_embed_attr([{_, ?dod     , V} | T], Equ) -> Equ1 = Equ#equ{dod      = Equ#equ.dod      + V}, set_embed_attr(T, Equ1);
set_embed_attr([{_, ?crit    , V} | T], Equ) -> Equ1 = Equ#equ{crit     = Equ#equ.crit     + V}, set_embed_attr(T, Equ1);
set_embed_attr([{_, ?crit_num, V} | T], Equ) -> Equ1 = Equ#equ{crit_num = Equ#equ.crit_num + V}, set_embed_attr(T, Equ1);
set_embed_attr([{_, ?crit_anti, V} | T], Equ) -> Equ1 = Equ#equ{crit_anti = Equ#equ.crit_anti + V}, set_embed_attr(T, Equ1);
set_embed_attr([{_, ?tou, V} | T], Equ) -> Equ1 = Equ#equ{tou = Equ#equ.tou + V}, set_embed_attr(T, Equ1);
set_embed_attr([{_, K, V} | T], Equ) ->
    ?ERR("Error Attr Id: ~w (~w)", [K, V]),
    set_embed_attr(T, Equ);
set_embed_attr([], Equ) -> Equ.

%% 血量、攻击 升级增加的属性= 英雄出生的初始值（不含1级所增加的数值）*(等级/30-等级*等级/3600)*0.75
%% 命中、闪避、暴击、抗暴、防御、穿刺、暴击提成  升级增加的属性= 英雄出生的初始值*(等级/30-等级*等级/3600)*0.75
get_combat_heroes1(Heroes, Items) ->
    F = fun(H) ->
            #hero{
                hp         = Hp
                ,lev       = Lev
                ,atk       = Atk
                ,def       = Def
                ,pun       = Pun
                ,hit       = Hit
                ,dod       = Dod
                ,crit      = Crit
                ,crit_num  = CritNum
                ,crit_anti = CritAnit
                ,tou       = Tou
            } = H,
            H1 = H#hero{
                hp         = util:floor(Hp       * (Lev/30 - Lev*Lev/3600) * 0.75 + Hp      )
                ,atk       = util:floor(Atk      * (Lev/30 - Lev*Lev/3600) * 0.75 + Atk     )
                ,def       = util:floor(Def      * Lev*Lev/3600 * 0.75 + Def     )
                ,pun       = util:floor(Pun      * Lev*Lev/3600 * 0.75 + Pun     )
                ,hit       = util:floor(Hit      * Lev*Lev/3600 * 0.75 + Hit     )
                ,dod       = util:floor(Dod      * Lev*Lev/3600 * 0.75 + Dod     )
                ,crit      = util:floor(Crit     * Lev*Lev/3600 * 0.75 + Crit    )
                ,crit_num  = util:floor(CritNum  * Lev*Lev/3600 * 0.75 + CritNum )
                ,crit_anti = util:floor(CritAnit * Lev*Lev/3600 * 0.75 + CritAnit)
                ,tou       = util:floor(Tou      * Lev*Lev/3600 * 0.75 + Tou     )
            },
            get_equs(H1, Items)
    end,
    Heroes1 = [F(Hero) || Hero <- Heroes, Hero#hero.pos > 0],
    case length(Heroes1) > 9 of
        true -> util:rand_element(9, Heroes1);
        false -> Heroes1
    end.

%% 血量、攻击 升级增加的属性= 英雄出生的初始值（不含1级所增加的数值）*(等级/30-等级*等级/3600)*0.75
%% 命中、闪避、暴击、抗暴、防御、穿刺、暴击提成  升级增加的属性= 英雄出生的初始值*(等级/30-等级*等级/3600)*0.75
get_combat_heroes(Heroes, Items) ->
    F = fun(H) ->
            #hero{
                hp         = Hp
                ,lev       = Lev
                ,atk       = Atk
                ,def       = Def
                ,pun       = Pun
                ,hit       = Hit
                ,dod       = Dod
                ,crit      = Crit
                ,crit_num  = CritNum
                ,crit_anti = CritAnit
                ,tou       = Tou
            } = H,
            H1 = H#hero{
                hp         = util:floor(Hp       * (Lev/30 - Lev*Lev/3600) * 0.75 + Hp      )
                ,atk       = util:floor(Atk      * (Lev/30 - Lev*Lev/3600) * 0.75 + Atk     )
                ,def       = util:floor(Def      * Lev*Lev/3600 * 0.75 + Def     )
                ,pun       = util:floor(Pun      * Lev*Lev/3600 * 0.75 + Pun     )
                ,hit       = util:floor(Hit      * Lev*Lev/3600 * 0.75 + Hit     )
                ,dod       = util:floor(Dod      * Lev*Lev/3600 * 0.75 + Dod     )
                ,crit      = util:floor(Crit     * Lev*Lev/3600 * 0.75 + Crit    )
                ,crit_num  = util:floor(CritNum  * Lev*Lev/3600 * 0.75 + CritNum )
                ,crit_anti = util:floor(CritAnit * Lev*Lev/3600 * 0.75 + CritAnit)
                ,tou       = util:floor(Tou      * Lev*Lev/3600 * 0.75 + Tou     )
            },
            get_equs(H1, Items)
    end,
    Heroes1 = [F(Hero) || Hero <- Heroes, Hero#hero.pos > 0],
    case length(Heroes1) > 9 of
        true -> util:rand_element(9, Heroes1);
        false -> Heroes1
    end.

clear_pos(Hero) ->
    case Hero#hero.pos > 0 of
        true -> Hero#hero{pos = 0, changed = 1};
        false -> Hero
    end.

set_pos(Rs, L) ->
    ?DEBUG("set_pos:~w", [L]),
    Hs = [clear_pos(Hero) || Hero <- Rs#role.heroes],
    case set_pos1(Hs, L) of
        {ok, Heroes} -> {ok, Rs#role{heroes = Heroes}};
        {error, Error} -> {error, Error}
    end.

set_pos1(Heroes, [[Id, Pos] | L]) ->
    case get_hero(Id, Heroes) of
        false ->
            ?WARN("set_pos error:~w, ~w", [Id, Pos]),
            {error, error};
        Hero ->
            %% ?INFO("set_pos:~w, ~w", [Id, Pos]),
            Hero1 = Hero#hero{pos = Pos, changed = 1},
            Heroes1 = set_hero(Hero1, Heroes),
            set_pos1(Heroes1, L)
    end;
set_pos1(Heroes, []) -> {ok, Heroes}.

%% set_skill(Pos, SkillId, Hero) ->
%%     Skills = Hero#hero.skills,
%%     PosData = lists:keyfind(Pos, 1, Skills),
%%     Data = data_skill:get(SkillId),
%%     Data1 = data_heroskill:get({Hero#hero.tid, Pos}),
%%     if
%%         Data == undefined ->
%%             {error, no_skill_data};
%%         Data1 == undefined ->
%%             ?INFO("data_heroskill:get({~w, ~w})", [Hero#hero.tid, SkillId]),
%%             {error, no_heroskill_data};
%%         PosData == false ->
%%             ?INFO("Pos:~w, Skills:~w", [Pos, Skills]),
%%             {error, no_pos};
%%         true ->
%%             Change = (util:get_val(change, Data1, 0) == 1),
%%             {_, OldId, _, _} = PosData,
%%             case Change == false andalso OldId > 0 of
%%                 true ->
%%                     {error, disable_change};
%%                 false ->
%%                     Rate = util:get_val(rate, Data, 0),
%%                     Tags = util:get_val(tags, Data, 0),
%%                     Skill = {Pos, SkillId, Rate, Tags},
%%                     Skills1 = lists:keystore(Pos, 1, Skills, Skill),
%%                     Hero1 = Hero#hero{skills = Skills1, changed = 1},
%%                     {ok, Hero1}
%%             end
%%     end.

get_hero(Id, Heroes) ->
    lists:keyfind(Id, 2, Heroes).

set_hero(Hero, Heroes) ->
    lists:keystore(Hero#hero.id, 2, Heroes, Hero).

%% add_hero(Rs) ->
%%     {ok, Id, Rs1} = lib_role:get_new_id(hero, Rs),
%%     Hero = init_hero(Id),
%%     db_insert(Rs#role.id, Hero),
%%     Rs1#role{heroes = [Hero | Rs#role.heroes]}.

%% get_hero_ids() ->
%%     Ids = data_hero:get(ids),
%%     get_hero_ids1(Ids, []).
%%
%% get_hero_ids1([Tid | Ids], Rt) ->
%%     Item = data_hero:get(Tid),
%%     case util:get_val(sort, Item) of
%%         1 -> get_hero_ids1(Ids, [Tid | Rt]);
%%         _ -> get_hero_ids1(Ids, Rt)
%%     end;
%% get_hero_ids1([], Rt) ->
%%     Rt.

add_hero_exp(Id, AddExp, Heroes) ->
    case get_hero(Id, Heroes) of
        false ->
            ?WARN("Error hero id: ~w", [Id]),
            Heroes;
        Hero ->
            Hero1 = add_hero_exp(Hero, AddExp),
            set_hero(Hero1, Heroes)
    end.

add_hero_exp(Hero, AddExp) ->
    Exp = Hero#hero.exp + AddExp,
    add_hero_exp1(Hero#hero{exp = Exp}).

add_hero_exp1(Hero) ->
    #hero{exp = Exp, exp_max = Max, lev = Lev} = Hero,
    LevMax = 200,
    case Exp >= Max andalso Lev < LevMax of
        true ->
            %% upgrade
            Lev1 = Lev + 1,
            self() ! {pt, 2010, [Lev1]},
            case util:get_val(exp_max, data_exp:get(Lev1)) of
                undefined ->
                    ?WARN("[No Lev:~w]", [Lev1]),
                    Hero;
                0 -> Hero;
                Max1 ->
                    Exp1 = Exp - Max,
                    Hero1 = Hero#hero{exp = Exp1, lev = Lev1, exp_max = Max1, changed = 1},
                    add_hero_exp1(Hero1)
            end;
        false -> Hero
    end.

get_tids_by_rare(Rare) ->
    Ids = data_hero:get(ids),
    lists:foldl(fun(Tid, Tids)->
                Data = data_hero:get(Tid),
                case util:get_val(rare, Data) of
                    Rare -> [Tid | Tids];
                    _ -> Tids
                end
        end, [], Ids).

attr_val(Attr, Data, Arg) ->
    V = util:get_val(Attr, Data, 0),
    %% Rand1 = util:rand(1, 100),
    %% Rand2 = util:rand(1, 100),
    %% util:ceil((Rand1*Rand2/10000*0.4*V+0.6*V)*Arg).
    util:ceil(V * data_fun:hero_offset()*Arg).

init_hero(Id) ->
    {Rare} = util:get_range_data(data_hero_rare),
    Tids = get_tids_by_rare(Rare),
    Tid = util:rand_element(Tids),
    {Quality, Arg} = util:get_range_data(data_hero_quality),
    case data_hero:get(Tid) of
        undefined ->
            ?WARN("[INIT HERO] Error hero id: ~w, Rare:~w", [Tid, Rare]),
            false;
        Data ->
            Job    = util:get_val(job    , Data),
            Lev = 1,
            DataExp  = data_exp:get(Lev),
            ExpMax   = util:get_val(exp_max, DataExp),
            Sort     = util:get_val(sort, Data),
            Hp       = attr_val(hp        , Data, Arg),
            Atk      = attr_val(atk        , Data, Arg),
            Def      = attr_val(def       , Data, Arg),
            Pun      = attr_val(pun       , Data, Arg),
            Hit      = attr_val(hit       , Data, Arg),
            Dod      = attr_val(dod       , Data, Arg),
            Crit     = attr_val(crit      , Data, Arg),
            CritNum  = attr_val(crit_num  , Data, Arg),
            CritAnit = attr_val(crit_anti , Data, Arg),
            Tou      = attr_val(tou       , Data, Arg),
            #hero{
                id = Id
                ,tid = Tid
                ,job = Job
                ,sort = Sort
                ,rare = Rare
                ,hp = Hp
                ,atk = Atk
                ,def       = Def
                ,pun       = Pun
                ,hit       = Hit
                ,dod       = Dod
                ,crit = Crit
                ,crit_num  = CritNum
                ,crit_anti = CritAnit
                ,tou       = Tou
                ,changed = 1
                ,exp_max = ExpMax
                ,exp = 0
                ,lev = Lev
                ,quality = Quality %% 品极
                ,skills = lib_role:init_skills(Data)
            }
    end.


init_hero(Id, Tid) ->
    Rare = 0,
    {Quality, Arg} = util:get_range_data(data_hero_quality),
    case data_hero:get(Tid) of
        undefined -> false;
        Data ->
            Job    = util:get_val(job    , Data),
            Lev = 1,
            DataExp  = data_exp:get(Lev),
            ExpMax   = util:get_val(exp_max, DataExp),
            Sort     = util:get_val(sort, Data),
            Hp       = attr_val(hp        , Data, Arg),
            Atk      = attr_val(atk        , Data, Arg),
            Def      = attr_val(def       , Data, Arg),
            Pun      = attr_val(pun       , Data, Arg),
            Hit      = attr_val(hit       , Data, Arg),
            Dod      = attr_val(dod       , Data, Arg),
            Crit     = attr_val(crit      , Data, Arg),
            CritNum  = attr_val(crit_num  , Data, Arg),
            CritAnit = attr_val(crit_anti , Data, Arg),
            Tou      = attr_val(tou       , Data, Arg),
            #hero{
                id = Id
                ,tid = Tid
                ,job = Job
                ,sort = Sort
                ,rare = Rare
                ,hp = Hp
                ,atk = Atk
                ,def       = Def
                ,pun       = Pun
                ,hit       = Hit
                ,dod       = Dod
                ,crit = Crit
                ,crit_num  = CritNum
                ,crit_anti = CritAnit
                ,tou       = Tou
                ,changed = 1
                ,exp_max = ExpMax
                ,exp = 0
                ,lev = Lev
                ,quality = Quality %% 品极
                ,skills = lib_role:init_skills(Data)
            }
    end.

%% 赠送英雄
give_hero(Id, Tid) ->
    %% {Rare} = util:get_range_data(data_hero_rare),
    %% Tids = get_tids_by_rare(Rare),
    %% Tid = util:rand_element(Tids),
    %% {Quality, Arg} = util:get_range_data(data_hero_quality),
	GiveData = data_hero_give:get(Tid),
	Rare = util:get_val(rare, GiveData),
	Quality = util:get_val(quality, GiveData),
	Arg = util:get_val(arg, GiveData),
	?DEBUG("Rare:~w,Quality:~w,Arg:~w", [Rare, Quality, Arg]),
    case data_hero:get(Tid) of
        undefined ->
            ?WARN("[INIT HERO] Error hero id: ~w, Rare:~w", [Tid, Rare]),
            false;
        Data ->
            Job      = util:get_val(job   , Data),
            Lev      = 1,
            DataExp  = data_exp:get(Lev),
            ExpMax   = util:get_val(exp_max, DataExp),
            Sort     = util:get_val(sort, Data),
            Hp       = attr_val(hp        , Data, Arg),
            Atk      = attr_val(atk       , Data, Arg),
            Def      = attr_val(def       , Data, Arg),
            Pun      = attr_val(pun       , Data, Arg),
            Hit      = attr_val(hit       , Data, Arg),
            Dod      = attr_val(dod       , Data, Arg),
            Crit     = attr_val(crit      , Data, Arg),
            CritNum  = attr_val(crit_num  , Data, Arg),
            CritAnit = attr_val(crit_anti , Data, Arg),
            Tou      = attr_val(tou       , Data, Arg),
            #hero{
                id = Id
                ,tid = Tid
                ,job = Job
                ,sort = Sort
                ,rare = Rare
                ,hp = Hp
                ,atk = Atk
                ,def       = Def
                ,pun       = Pun
                ,hit       = Hit
                ,dod       = Dod
                ,crit = Crit
                ,crit_num  = CritNum
                ,crit_anti = CritAnit
                ,tou       = Tou
                ,changed = 1
                ,exp_max = ExpMax
                ,exp = 0
                ,lev = Lev
                ,quality = Quality %% 品极
                ,skills = lib_role:init_skills(Data)
            }
    end.

get_quality_arg(Q) ->
    Ids = data_hero_quality:get(ids),
    get_quality_arg(Ids, Q).

get_quality_arg([Id | Ids], Q) ->
    case data_hero_quality:get(Id) of
        {Q, Arg} -> Arg;
        _ -> get_quality_arg(Ids, Q)
    end;
get_quality_arg([], Q) ->
    ?WARN("Undefined quality:~w", [Q]),
    undefined.

upgrade_quality(Hero) ->
    #hero{
        hp        = Hp
        ,atk       = Atk
        ,def       = Def
        ,pun       = Pun
        ,hit       = Hit
        ,dod       = Dod
        ,crit      = Crit
        ,crit_num  = CritNum
        ,crit_anti = CritAnit
        ,tou       = Tou
        ,quality = Quality
    } = Hero,
    Arg1 = get_quality_arg(Quality),
    Arg2 = get_quality_arg(Quality + 1),
    NewHp       = util:ceil(Hp       / Arg1 * Arg2),
    NewAtk      = util:ceil(Atk      / Arg1 * Arg2),
    NewDef      = util:ceil(Def      / Arg1 * Arg2),
    NewPun      = util:ceil(Pun      / Arg1 * Arg2),
    NewHit      = util:ceil(Hit      / Arg1 * Arg2),
    NewDod      = util:ceil(Dod      / Arg1 * Arg2),
    NewCrit     = util:ceil(Crit     / Arg1 * Arg2),
    NewCritNum  = util:ceil(CritNum  / Arg1 * Arg2),
    NewCritAnit = util:ceil(CritAnit / Arg1 * Arg2),
    NewTou      = util:ceil(Tou      / Arg1 * Arg2),
    Hero#hero{
        hp         = NewHp
        ,atk       = NewAtk
        ,def       = NewDef
        ,pun       = NewPun
        ,hit       = NewHit
        ,dod       = NewDod
        ,crit      = NewCrit
        ,crit_num  = NewCritNum
        ,crit_anti = NewCritAnit
        ,tou       = NewTou
        ,quality   = Quality + 1
    }.

%%' 酒馆刷新英雄
tavern_refresh(Tavern) ->
    tavern_refresh([1, 2, 3], Tavern).

tavern_refresh([Id | Ids], Tavern) ->
    case lists:keyfind(Id, 1, Tavern) of
        false ->
            Hero = init_hero(0),
            tavern_refresh(Ids, [{Id, 0, Hero} | Tavern]);
        {_, 0, _} ->
            Hero = init_hero(0),
            Tavern1 = lists:keyreplace(Id, 1, Tavern, {Id, 0, Hero}),
            tavern_refresh(Ids, Tavern1);
        {_, 1, _} ->
            tavern_refresh(Ids, Tavern)
    end;
tavern_refresh([], Tavern) ->
    Tavern.
%%.

%%' 数据库相关操作

db_init_tavern(Rs) ->
    Sql = list_to_binary([<<"select tavern_time, tavern from tavern where role_id = ">>, integer_to_list(Rs#role.id)]),
    case db:get_row(Sql) of
        {ok, [Time, Tavern]} ->
            {ok, Rs#role{
                tavern_time = Time
                ,tavern = unzip_tavern(Tavern)
            }};
        {error, null} ->
            Time = util:unixtime(),
            Tavern = tavern_refresh(Rs#role.tavern),
            Rs1 = Rs#role{
                tavern_time = Time
                ,tavern = Tavern
            },
            case db_insert_tavern(Rs1) of
                {ok, _} -> {ok, Rs1};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            %% 初始化失败
            {error, Reason}
    end.

db_insert_tavern(Rs) ->
    #role{id = Rid, tavern_time = Time, tavern = Tavern} = Rs,
    db:execute("insert into tavern (role_id, tavern_time, tavern) values (~s, ~s, ~s)",
        [Rid, Time, zip_tavern(Tavern)]).

db_update_tavern(Rs) ->
    #role{id = Rid, tavern_time = Time, tavern = Tavern} = Rs,
    db:execute("update tavern set tavern_time = ~s, tavern = ~s where role_id = ~s", [Time, zip_tavern(Tavern), Rid]).


-spec db_init(Rs) -> {ok, NewRs} | {error, Reason} when
    Rs :: #role{},
    NewRs :: #role{},
    Reason :: term().

db_init(Rs) ->
    Rid = Rs#role.id,
    Sql = list_to_binary([
            <<"select hero_id, val from hero where role_id = ">>
            ,integer_to_list(Rid)
        ]),
    case db:get_all(Sql) of
        {ok, Rows} ->
            {MaxId, Heroes} = db_init1(Rows, []),
            {ok, Rs#role{heroes = Heroes, max_hero_id = MaxId}};
        {error, null} ->
            {ok, Rs#role{heroes = []}};
        {error, Reason} ->
            {error, Reason}
    end.

db_init1([H | T], Result) ->
    db_init1(T, [unzip(H) | Result]);
db_init1([], Result) ->
    MaxId = lists:max([H#hero.id || H <- Result]),
    {MaxId, Result}.

db_delete(Rid, Hid) when is_integer(Hid) ->
    Sql = list_to_binary([
            <<"delete from hero where role_id = ">>
            ,integer_to_list(Rid),<<" and hero_id = ">>
            ,integer_to_list(Hid)
        ]),
    db:execute(Sql);
db_delete(Rid, H) ->
    db_delete(Rid, H#hero.id).

-spec db_insert(Rid, Hero) -> {ok, Num} | {error, Reason} when
    Rid :: integer(),
    Hero :: #hero{},
    Num :: integer(), %% 影响行数
    Reason :: term().

db_insert(Rid, H) ->
    ?DEBUG("H:~w", [H]),
    Val = ?ESC_SINGLE_QUOTES(zip(H)),
    RidL = integer_to_list(Rid),
    HidL = integer_to_list(H#hero.id),
    Sql = list_to_binary([
            <<"SELECT count(*) FROM `hero` WHERE role_id = ">>,
            RidL,<<" and hero_id = ">>,HidL
        ]),
    case db:get_one(Sql) of
        {error, null} ->
            Sql2 = list_to_binary([
                    <<"insert hero (role_id, hero_id, val) value (">>
                    ,RidL,<<",">>,HidL,<<",'">>,Val,<<"')">>
                ]),
            db:execute(Sql2);
        {error, Reason} ->
            {error, Reason};
        {ok, _Num} ->
            %% 记录已存在
            {ok, 0}
    end.

-spec db_update(Rid, Heroes) -> {true, NewHeroes} | {false, NewHeroes} when
    Rid :: integer(),
    Heroes :: NewHeroes,
    NewHeroes :: [#hero{}].

db_update(Rid, HL) ->
    db_update(Rid, HL, []).

db_update(Rid, [H | T], Result) ->
    Result1 = [H#hero{changed = 0} | Result],
    case H#hero.changed of
        1 ->
            Val = ?ESC_SINGLE_QUOTES(zip(H)),
            Sql = list_to_binary([
                    <<"update hero set val = '">>,Val,<<"' where ">>
                    ,<<" role_id = ">>,integer_to_list(Rid)
                    ,<<" and hero_id = ">>,integer_to_list(H#hero.id)
                ]),
            case db:execute(Sql) of
                {ok, 0} ->
                    %% 影响行数为0，可能是记录不存在，进行插入操作
                    case db_insert(Rid, H) of
                        {ok, _} ->
                            db_update(Rid, T, Result1);
                        {error, _} ->
                            {false, Result ++ [H | T]}
                    end;
                {ok, 1} ->
                    db_update(Rid, T, Result1);
                {ok, Num} ->
                    ?WARN("Update Hero Return Rows: ~w", [Num]),
                    db_update(Rid, T, Result1);
                {error, Reason} ->
                    ?WARN("Update Hero Error: ~w", [Reason]),
                    {false, Result ++ [H | T]}
            end;
        0 ->
            db_update(Rid, T, Result1);
        _ ->
            ?ERR("Undefined #hero.changed: ~w", [H#hero.changed]),
            db_update(Rid, T, Result1)
    end;
db_update(_Rid, [], Result) -> {true, Result}.
%%.

zip_tavern(Tavern) ->
    zip_tavern(Tavern, <<>>).

zip_tavern([{Nth, Lock, H} | Tavern], Rt) ->
    Bin = zip(H),
    Size = byte_size(Bin),
    Rt1 = <<Rt/binary, Nth:8, Lock:8, Size:16, Bin/binary>>,
    zip_tavern(Tavern, Rt1);
zip_tavern([], Rt) ->
    Rt.

unzip_tavern(Bin) ->
    unzip_tavern(Bin, []).

unzip_tavern(<<Nth:8, Lock:8, Size:16, HeroBin:Size/binary, RestBin/binary>>, Rt) ->
    Hero = unzip([0, HeroBin]),
    Rt1 = [{Nth, Lock, Hero} | Rt],
    unzip_tavern(RestBin, Rt1);
unzip_tavern(<<>>, Rt) -> Rt.

%%' zip / unzip
unzip([Id, Val]) ->
    <<Version:8, Bin1/binary>> = Val,
    case Version of
        1 ->
            <<Tid:32, Job:8, Sort:8, Rare:8,
            Hp      :32,
            Atk     :32,
            Def     :32,
            Pun     :32,
            Hit     :32,
            Dod     :32,
            Crit    :32,
            CritNum :32,
            CritAnit:32,
            Tou     :32,
            Quality :8,
            Pos:8, ExpMax:32, Exp:32, Lev:8, Step:8
            >> = Bin1,
            %% ?INFO("unzip step:~w, skills:~w", [Step, Skills]),
            Data = data_hero:get(Tid),
            #hero{
                id         = Id
                ,tid       = Tid
                ,job       = Job
                ,sort      = Sort
                ,rare      = Rare
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
                ,pos       = Pos
                ,exp_max   = ExpMax
                ,exp       = Exp
                ,lev       = Lev
                ,step      = Step
                ,quality   = Quality
                ,skills = lib_role:init_skills(Data)
            };
        _ ->
            ?ERR("undefined version: ~w", [Version]),
            undefined
    end.

-define(HERO_ZIP_VERSION, 1).

zip(H) ->
    #hero{
        tid        = Tid
        ,job       = Job
        ,sort      = Sort
        ,rare      = Rare
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
        ,pos       = Pos
        ,exp_max   = ExpMax
        ,exp       = Exp
        ,lev       = Lev
        ,step      = Step
        ,quality   = Quality
    } = H,
    <<?HERO_ZIP_VERSION:8, Tid:32, Job:8, Sort:8, Rare:8,
    Hp      :32,
    Atk     :32,
    Def     :32,
    Pun     :32,
    Hit     :32,
    Dod     :32,
    Crit    :32,
    CritNum :32,
    CritAnit:32,
    Tou     :32,
    Quality :8,
    Pos:8, ExpMax:32, Exp:32, Lev:8, Step:8
    >>.
%%.

%% vim: set filetype=erlang foldmarker=%%',%%. foldmethod=marker:
