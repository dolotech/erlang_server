%%----------------------------------------------------
%% $Id: mod_battle_fun.erl 5386 2013-11-28 12:17:58Z rolong $
%%
%% 战斗模块
%%----------------------------------------------------

-module(mod_battle_fun).
-compile(export_all).
%% -export([
%%     ]
%% ).

-include("common.hrl").
-include("hero.hrl").
-include("s.hrl").

set_cmd([S | Targets], A, {Trigger, T, Cmd}, ExtArg, Rt) ->
    Skills = [[{trigger, Trigger}, {target, T}, {cmd, [Cmd]}] | S#s.skills],
    S1 = S#s{skills = Skills},
    Rt1 = [S1 | Rt],
    set_cmd(Targets, A, {Trigger, T, Cmd}, ExtArg, Rt1);
set_cmd([], _, _, _, Rt) -> Rt.

add_hp([S | Targets], A, {Op, V}, ExtArg, Rt) ->
    Hp = case Op of
        '*' -> S#s.hp + S#s.hp_max * V;
        '+' -> S#s.hp + V;
        '-' -> max(S#s.hp - V, 0);
        cure ->
            X = mod_battle:calc_crit(A, S),
            XX = S#s.hp + A#s.atk * mod_battle:calc_dmg_offset() * X * V,
            %% ?INFO("CURE: ~w + ~w = ~w [crit:~w,atk:~w, V:~w]", [S#s.hp, XX - S#s.hp, XX, X, S#s.atk, V]),
            XX
    end,
    %% S1 = if 
    %%     Op =:= cure -> set_state(S, ?STA_CURE);
    %%     true -> S 
    %% end,
    S1 =  set_state(S, ?STA_CURE),
    S2 = if 
        S#s.hp =< 0, Hp > 0 -> 
                 set_state(S1, ?STA_REALIVE);
        true -> S1
    end,
    Hp1 = min(util:ceil(Hp), S2#s.hp_max),
    S3 = S2#s{hp = Hp1},
    Rt1 = [S3 | Rt],
    add_hp(Targets, A, {Op, V}, ExtArg, Rt1);
add_hp([], _, _, _, Rt) -> Rt.

set_dod([S | Targets], A, {Op, V}, ExtArg, Rt) ->
    New = case Op of
             '-' -> max(S#s.dod - V, 0);
             '+' -> S#s.dod + V;
             '*' -> S#s.dod * V
         end,
    S1 = S#s{dod = New},
    Rt1 = [S1 | Rt],
    set_dod(Targets, A, {Op, V}, ExtArg, Rt1);
set_dod([], _, _, _, Rt) -> Rt.

set_hit([S | Targets], A, {Op, V}, ExtArg, Rt) ->
    New = case Op of
             '-' -> max(S#s.hit - V, 0);
             '+' -> S#s.hit + V;
             '*' -> S#s.hit * V
         end,
    S1 = S#s{hit = New},
    Rt1 = [S1 | Rt],
    set_hit(Targets, A, {Op, V}, ExtArg, Rt1);
set_hit([], _, _, _, Rt) -> Rt.

set_def([S | Targets], A, {Op, V}, ExtArg, Rt) ->
    New = case Op of
             '-' -> max(S#s.def - V, 0);
             '+' -> S#s.def + V;
             '*' -> S#s.def * V
         end,
    S1 = S#s{def = New},
    Rt1 = [S1 | Rt],
    set_def(Targets, A, {Op, V}, ExtArg, Rt1);
set_def([], _, _, _, Rt) -> Rt.

set_pun([S | Targets], A, {Op, V}, ExtArg, Rt) ->
    New = case Op of
             '+' -> S#s.pun + V;
             '-' -> max(S#s.pun - V, 0);
             '*' -> S#s.pun * V
         end,
    S1 = S#s{pun = New},
    Rt1 = [S1 | Rt],
    set_pun(Targets, A, {Op, V}, ExtArg, Rt1);
set_pun([], _, _, _, Rt) -> Rt.

set_atk([S | Targets], A, {Op, V}, ExtArg, Rt) ->
    New = case Op of
             '+' -> S#s.atk + V;
             '-' -> max(S#s.atk - V, 0);
             '*' -> S#s.atk * V
         end,
    S1 = S#s{atk = New},
    Rt1 = [S1 | Rt],
    set_atk(Targets, A, {Op, V}, ExtArg, Rt1);
set_atk([], _, _, _, Rt) -> Rt.

add_atk([S | Targets], A, {Op, V}, ExtArg, Rt) ->
    Add = case Op of
             '+' -> V;
             '*' -> S#s.atk * V
         end,
    S1 = S#s{atk = Add + S#s.atk},
    Rt1 = [S1 | Rt],
    add_atk(Targets, A, {Op, V}, ExtArg, Rt1);
add_atk([], _, _, _, Rt) -> Rt.

sub_atk([S | Targets], A, {Op, V}, ExtArg, Rt) ->
    Add = case Op of
             '-' -> V;
             '*' -> S#s.atk * V
         end,
    S1 = S#s{atk = max(S#s.atk - Add, 0)},
    Rt1 = [S1 | Rt],
    sub_atk(Targets, A, {Op, V}, ExtArg, Rt1);
sub_atk([], _, _, _, Rt) -> Rt.

set_crit_anti([S | Targets], A, {Op, V}, ExtArg, Rt) ->
    New = case Op of
             '+' -> S#s.crit_anti + V;
             '*' -> S#s.crit_anti * V
         end,
    S1 = S#s{crit_anti = New},
    Rt1 = [S1 | Rt],
    set_crit_anti(Targets, A, {Op, V}, ExtArg, Rt1);
set_crit_anti([], _, _, _, Rt) -> Rt.

set_buff([S | Targets], A, {Name, Args, Bout, Id, Trigger}, ExtArg, Rt) ->
    Skip1 = case Args of
        {rate, SetRate} -> not util:rate(SetRate);
        _ -> false
    end,
    Anti = mod_battle:get_skill_cmd_value_by_key(S, set_buff, anti_buff),
    Skip2 = is_list(Anti) andalso lists:member(Id, Anti),
    case Skip1 orelse Skip2 of
        true ->
            set_buff(Targets, A, {Name, Args, Bout, Id, Trigger}, ExtArg, [S|Rt]);
        false ->
            #s{buffs = Buffs, buff_ids = BuffIds} = S,
            Buff = #buff{
                name = Name
                ,args = Args
                ,bout = Bout 
                ,id = Id 
                ,trigger = Trigger 
            },
            Buffs1 = lists:keystore(
                Name, #buff.name, Buffs, Buff),
            BuffIds1 = case Id == 0 orelse lists:member(Id, BuffIds) of
                true -> BuffIds;
                false -> [Id | BuffIds]
            end,
            S1 = S#s{buffs = Buffs1, buff_ids = BuffIds1},
            Rt1 = [S1 | Rt],
            set_buff(Targets, A, {Name, Args, Bout, Id, Trigger}, ExtArg, Rt1)
    end;
set_buff([], _, _, _, Rt) -> Rt.

%% 将所受的伤害比转化为攻击
%% atked2atk([S | Targets], A, V, Atked, Rt) ->
%%     Rt1 = case Atked > 0 of
%%         true ->
%%             #s{hp_max = HpMax, atk = Atk} = S,
%%             Rate = Atked / HpMax / V + 1,
%%             AtkAdd = util:ceil(Atk * Rate),
%%             [S#s{atk = AtkAdd} | Rt];
%%         false -> 
%%             [S | Rt]
%%     end,
%%     atked2atk(Targets, A, V, Atked, Rt1);
%% atked2atk([], _, _, _, Rt) ->
%%     Rt.
%%增加光环后的攻击=基础攻击+基础攻击*（1-当前血量比例）/表内的值
atked2atk([S | Targets], A, V, _Atked, Rt) ->
    #s{hp_max = HpMax, hp = Hp, atk = Atk} = S,
    Rt1 = case HpMax > Hp of
        true ->
            Add = util:ceil(Atk * (1 - Hp / HpMax) * V),
            Atk1 = Atk + Add,
            %% ?INFO("atked2atk: ~w + ~w = ~w, Hp: ~w/~w", [Atk, Add, Atk1, Hp, HpMax]),
            [S#s{atk = Atk1} | Rt];
        false -> 
            [S | Rt]
    end,
    atked2atk(Targets, A, V, _Atked, Rt1);
atked2atk([], _, _, _, Rt) ->
    Rt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_states(Ss, V) ->
    set_states(Ss, V, []).

set_states([S | Ss], V, Rt) ->
    S1 = set_state(S, V),
    set_states(Ss, V, [S1 | Rt]);
set_states([], _V, Rt) ->
    lists:reverse(Rt).

set_state(S, 0) ->
    S#s{state = 0};
set_state(S, V) ->
    S#s{state = S#s.state bor V}.

