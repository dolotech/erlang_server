%%----------------------------------------------------
%% $Id: mod_battle.erl 5669 2013-12-05 07:58:34Z rolong $
%%
%% 战斗模块
%%----------------------------------------------------

-module(mod_battle).
-export([
        battle/0
        ,battle/2
        ,print_battle/1
        ,print_hero/1
        ,calc_dmg_offset/0
        ,calc_crit_num/1
        ,calc_crit/2
        ,get_skill_by_trigger/2
        ,get_skill_cmd_value_by_key/3
    ]
).

-include("common.hrl").
-include("hero.hrl").
-include("s.hrl").

%%----------------------------------------------------
%% API
%%----------------------------------------------------

%% get_skill_cmd_value_by_key
-spec get_skill_cmd_value_by_key(Soldier, TriggerKey, CmdKey) -> false | Value when
      Soldier :: #s{},
      TriggerKey :: atom(),
      CmdKey :: atom(),
      Value :: term().

get_skill_cmd_value_by_key(Soldier, TriggerKey, CmdKey) ->
    case get_skill_by_trigger(Soldier, TriggerKey) of
        false -> false;
        Skill ->
            case util:get_val(cmd, Skill) of
                undefined -> false;
                Cmd -> util:get_val(CmdKey, Cmd, false)
            end
    end.

%%' get_skill_by_trigger
-spec get_skill_by_trigger(Soldier, TriggerKey) -> false | Skill when
      Soldier :: #s{},
      TriggerKey :: atom(),
      Skill :: list().

get_skill_by_trigger(Soldier, TriggerKey) ->
    get_skill_by_trigger(Soldier#s.skills, Soldier, TriggerKey).

get_skill_by_trigger([Skill | Skills], S, TriggerKey) ->
    case util:get_val(trigger, Skill) of
        {TriggerKey, Arg} -> 
            case eval_trigger_arg(Arg, S) of
                true -> Skill;
                false -> false
            end;
        _ -> get_skill_by_trigger(Skills, S, TriggerKey)
    end;
get_skill_by_trigger([], _, _) -> false.

eval_trigger_arg({self_hp_less, V}, {A, _}) ->
    A#s.hp < V;
eval_trigger_arg({rate, V}, _) ->
    util:rate(V);
eval_trigger_arg(dizzy, {_, B}) ->
    lists:keymember(dizzy, #buff.name, B#s.buffs);
eval_trigger_arg(ok, _S) -> true;
eval_trigger_arg(wake, _S) -> true;
eval_trigger_arg({hp_ratio_less, V}, S) ->
    #s{hp = Hp, hp_max = HpMax} = S, 
    %% ?INFO("Pos:~w, ~w/~w=~w, ~w/100", [S#s.pos, Hp, HpMax, Hp/HpMax, V]),
    (Hp / HpMax) < (V / 100);
eval_trigger_arg(Arg, S) -> 
    ?WARN("eval_trigger_arg, arg:~w S:~w", [Arg, S]),
    false.
%%.

%%----------------------------------------------------
%% API
%%----------------------------------------------------
battle() ->
    AL = init_s_a(),
    BL = init_s_b(),
    battle(AL, BL).

battle(AL, BL) ->
    print_hero(AL ++ BL),
    SA = init_soldiers(AL),
    SB = init_soldiers(BL),
    {SA1, SB1} = select_init_skill(SA, SA, SB),
    {SB2, SA2} = select_init_skill(SB1, SB1, SA1),
    Rt = bouts(SA2, SB2),
    print_battle(Rt),
    Rt.

init_soldiers(Heroes) ->
    init_soldiers(Heroes, []).

init_soldiers([], Rt) -> Rt;
init_soldiers([H | Heroes], Rt) when H#hero.pos > 0 ->
    #hero{
       job  = Job
       ,pos = Pos
       ,hp = Hp
       ,atk       = Atk     
       ,def       = Def     
       ,pun       = Pun     
       ,hit       = Hit     
       ,dod       = Dod     
       ,crit      = Crit
       ,crit_num  = CritNum
       ,crit_anti = CritAnit
       ,tou       = Tou
       ,tou_anit  = TouAnit     
       ,skills    = Skills0
      } = H,
    %% SkillIds = util:rand_element(util:rand(0, 4), [50001, 50002, 50004, 50005]),
    Skills = [[{id, X}] ++ data_skill:get(X) || X <- Skills0],
    %% io:format("~n~w", [Skills]),
    S = #s{
           job = Job
           ,pos = Pos
           ,hp_max = Hp
           ,hp = Hp
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
           %% ,status = 0
           ,skills = Skills
          },
    init_soldiers(Heroes, [S|Rt]);
init_soldiers([_ | Heroes], Rt) ->
    init_soldiers(Heroes, Rt).

init_s_a() ->
    [
     {hero,0,40002,0,0,0,0,2000,500,100,100,100,100,100,100,100,100,101,0,29,0,0,0,0,1,[7,8,9]}
     ,{hero,0,40002,0,0,0,0,2000,500,100,100,100,100,100,100,100,100,101,0,27,0,0,0,0,1,[7,8,9]}
     ,{hero,0,40003,0,0,0,0,6000,800,100,100,100,100,100,100,100,100,102,0,25,0,0,0,0,1,[10,11,12]}
     ,{hero,0,40002,0,0,0,0,2000,500,100,100,100,100,100,100,100,100,101,0,23,0,0,0,0,1,[7,8,9]}
     ,{hero,0,40002,0,0,0,0,2000,500,100,100,100,100,100,100,100,100,101,0,21,0,0,0,0,1,[7,8,9]}
    ].

init_s_b() ->
    [
     {hero,2,30006,1,6,3,1,5291,1144,100,100,100,100,100,100,100,100,0,0,14,44,0,1,0,6,[]}
     ,{hero,3,30007,3,7,1,1,5088,1100,100,100,100,100,100,100,100,100,0,0,13,44,0,1,0,5,[]}
     ,{hero,7,30005,1,7,3,1,4479,970,100,100,100,100,100,100,100,100,0,0,12,44,0,1,0,2,[]}
     ,{hero,10,30004,1,4,1,1,4400,1100,100,100,100,100,100,100,100,100,0,0,11,44,0,1,0,5,[]}
    ].

%% first_atk(AL, BL) ->
%%     L = [1, 2, 3, 4, 5, 6, 7, 8, 9],
%%     first_atk(L, AL, BL).
%% 
%% first_atk([I | L], AL, BL) ->
%%     case lists:keymember(I+10, #s.pos, AL) of
%%         true -> 1;
%%         false ->
%%             case lists:keymember(I+20, #s.pos, BL) of
%%                 true -> 2;
%%                 false -> first_atk(L, AL, BL)
%%             end
%%     end;
%% first_atk([], _AL, _BL) -> 1.

%%' 轮流出手，默认攻方先手，返回战斗过程指令
-spec bouts(AL, BL) -> Result when
    AL :: [#s{}],
    BL :: [#s{}],
    Result :: list().

bouts(AL, BL) ->
    %% Index = first_atk(AL, BL),
    bouts([], 1, 0, AL, BL, []).

bouts([CurPos | Order], Index, Bout, AL0, BL0, Result) ->
    IsOver = is_over(Index, Bout, AL0, BL0),
    if 
        IsOver > 0 ->
            %% 战斗结束
            {IsOver, lists:reverse(Result)};
        true ->
            %% {_Bout1, AL, BL} = set_status(Bout, AL0, BL0),
            AL = AL0,
            BL = BL0,
            Index1 = Index + 1,
            %% 默认第一回合为非技能回合
            %% Active: true为技能回合，false为非技能回合
            Active = case Bout rem 2 of
                1 -> false;
                0 -> true
            end,
            %% io:format("index:~w, bout:~w~n", [Index, Bout]),
            case CurPos div 10 of
                1 ->
                    {AL1, BL1, Re} = assign_soldiers(CurPos, Active, AL, BL),
                    Result1 = if Re == [] -> Result; 
                                 true -> [Re | Result] end,
                    bouts(Order, Index1, Bout, AL1, BL1, Result1);
                2 ->
                    {BL1, AL1, Re} = assign_soldiers(CurPos, Active, BL, AL),
                    Result1 = if Re == [] -> Result;
                                 true -> [Re | Result] end,
                    bouts(Order, Index1, Bout, AL1, BL1, Result1)
            end
    end;
bouts([], Index, Bout, AL0, BL0, Result) ->
    %% let i=1|'<,'>g/X/s//\=i/g|let i=i+1
    Order = [
        11, 21, 
        12, 22,
        13, 23,
        14, 24,
        15, 25,
        16, 26,
        17, 27,
        18, 28,
        19, 29
    ],
    bouts(Order, Index, Bout + 1, AL0, BL0, Result).
%%.

%%' 选择攻击者和被攻击者
-spec assign_soldiers(CurPos, Active, AL, BL) -> {NewAL, NewBL, Result} when
    CurPos :: integer(),
    Active :: true | false, %% true为技能回合，false为非技能回合
    AL :: BL,
    BL :: NewAL,
    NewAL :: NewBL,
    NewBL :: [#s{}],
    Result :: list(). %% 一个小回合的一条战斗指令

assign_soldiers(CurPos, Active, AL0, BL0) ->
    AL = mod_battle_fun:set_states(AL0, 0),
    BL = mod_battle_fun:set_states(BL0, 0),
    case select_atk(CurPos, AL) of
        {next, AL1} -> 
            {AL1, BL, []};
        {ok, AL1, Pos} ->
            case select_skill(Active, Pos, AL1, BL) of
                {false, AL2} ->
                    %% 非技能回合，按正常顺序选择一个目标
                    Poses = normal_pos(Pos),
                    case select_target_by_pos(Poses, 1, BL, []) of
                        [] -> {AL2, BL, []};
                        AtkedList ->
                            {AL3, BL1, Re} = atk_targets(Pos, AL2, BL, AtkedList, 1, 0),
                            {AL3, BL1, Re}
                    end;
                {Targets, Skill, SkillType} ->
                    SkillId = util:get_val(id, Skill, 0),
                    Cmd = util:get_val(cmd, Skill, []),
                    A = get_s(Pos, AL1),
                    Targets1 = eval_cmd(Cmd, A, Targets),
                    case util:get_val(atk, Skill, 0) of
                        0 ->
                            {AL2, BL1} = set_ssss(Targets1, AL1, BL),
                            AL3 = set_used_wake_skill(SkillType, Pos, AL2),
                            ReT = [[X1, X2, State, X3] || 
                                #s{pos = X1, hp = X2, state = State, buff_ids = X3} 
                                            <- Targets1],
                            Re = [A#s.pos, A#s.buff_ids, ReT, SkillId],
                            {AL3, BL1, Re};
                        Atk ->
                            {AL2, BL2, Re} = atk_targets(Pos, AL1, BL, Targets1, Atk, SkillId),
                            AL3 = set_used_wake_skill(SkillType, Pos, AL2),
                            {AL3, BL2, Re}
                    end
            end
    end.
%%.

%% 对所有目标进行一次攻击
atk_targets(Pos, AL, BL, Targets, AtkArg, SkillId) ->
    A = get_s(Pos, AL),
    %% A1 = A#s{status = 1},
    A1 = A,
    {A2, Targets1, ResultBL} = atk_target(A1, SkillId, Targets, AtkArg, [], []),
    #s{buff_ids = BuffIds} = A2,
    BL1 = set_ss(Targets1, BL),
    AL1 = set_s(A2, AL),
    BL2 = do_clean(ResultBL, BL1),
    Result = [Pos, BuffIds, ResultBL, SkillId],
    {AL1, BL2, Result}.

do_clean([[Pos, 0, _, _] | Re], AL) ->
    %% ?INFO("DIE:~w", [Pos]),
    S = get_s(Pos, AL),
    AL1 = case get_skill_cmd_value_by_key(S, init, set_cmd) of
              {{die, ok}, self, _} ->
                  del_die_skill(AL, Pos, []);
              _ -> AL
          end,
    do_clean(Re, AL1);
do_clean([_ | Re], AL) ->
    do_clean(Re, AL);
do_clean([], AL) -> 
    AL.

del_die_skill([S | Ss], Pos, Rt) ->
    S1 = case S#s.pos =/= Pos of
             true -> 
                 F = fun
                         ([{trigger, {die, ok}}, _, _]) -> false;
                         (_) -> true
                     end,
                 Skills = lists:filter(F, S#s.skills),
                 S#s{skills = Skills};
             false ->
                 S
         end,
    del_die_skill(Ss, Pos, [S1 | Rt]);
del_die_skill([], _, Rt) -> Rt.

atk_target(A01, SkillId, [B01 | BL], AtkArg, NewBL, Result) ->
    %% 解析攻击时触发的技能
    {A, B} = select_atk_skill(A01, B01),
    BuffIds = B#s.buff_ids,
    %% 解析攻击时触发的BUFF
    {A1, A002} = do_atk_buff(A#s.buffs, A, A, length(BL)),
    {A2, B1, Result1} = atk_target1(A1, B, A002, AtkArg, SkillId, BuffIds, Result),
    NewBL1 = [B1 | NewBL],
    atk_target(A2, SkillId, BL, AtkArg, NewBL1, Result1);
atk_target(A, _SkillId, [], _AtkArg, NewBL, Result) -> 
    {A, NewBL, lists:reverse(Result)}.

%% 攻击
atk_target1(A, B, A01, AtkArg, SkillId, BuffIds, Rt) ->
    #s{hp = Hp, atked = Atked, pos = Pos, state = State} = B,
    {State1, Atk} = case calc_hit(A01, B) of
                         0 -> {State bor ?STA_DOD, 0}; %% 闪
                         1 -> 
                             Crit = calc_crit(A01, B),
                             StateT1 = case Crit > 1 of
                                            true -> State bor ?STA_CRIT;
                                            false -> State
                                        end,
                             AtkT1 = calc_atk(A01, B, Crit, AtkArg),
                             {StateT1, AtkT1}
                     end,
    %% ?INFO("atk ~w ~w sta:~w, atkarg:~w", [A#s.atk, Atk, State, AtkArg]),
    %% Maybe Die
    Hp1 = min0(Hp - Atk),
    HpAtked = Hp - Hp1,
    B1 = B#s{hp = Hp1, atked = Atked + HpAtked, state = State1},
    %% 解析被攻击后触发的技能
    {B2, A2} = case Hp1 > 0 of
                   true -> select_atked_skill(B1#s.skills, HpAtked, B1, A);
                   false -> {B1, A}
               end,
    {B3, A3} = case Hp1 =< 0 of
                   true -> select_die_skill(B2#s.skills, HpAtked, B2, A2);
                   false -> {B2, A2}
               end,
    {B4, A4} = case SkillId > 0 of
                   true -> select_skill_atked_skill(B3#s.skills, HpAtked, B3, A3);
                   false -> {B3, A3}
               end,
    Rt1 = [[Pos, B4#s.hp, B4#s.state, BuffIds] | Rt],
    {A4, B4, Rt1}.

get_buff(Name, Buffs) ->
    lists:keyfind(Name, #buff.name, Buffs).

sub_buff(Buff, S) ->
    %% ?INFO("~w, Buffs:~w, ~8.2.0B", [S#s.pos, S#s.buffs, S#s.buff_status]),
    case Buff#buff.bout > 1 of
        true -> 
            Buff1 = Buff#buff{bout = Buff#buff.bout - 1},
            Buffs1 = lists:keyreplace(
                       Buff1#buff.name, #buff.name, S#s.buffs, Buff1),
            %% ?INFO("Buffs:~w", [Buffs1]),
            S#s{buffs = Buffs1};
        false ->
            Buffs1 = lists:keydelete(Buff#buff.name, #buff.name, S#s.buffs),
            BuffIds = lists:delete(Buff#buff.id, S#s.buff_ids),
            %% ?INFO("~w, Buffs:~w, ~8.2.0B", [S#s.pos, Buffs1, Status1]),
            S#s{buffs = Buffs1, buff_ids = BuffIds}
    end.


%% 解析目标指令
eval_target({Type, Num}, AS, AL, BL) ->
    eval_target({Type, Num, normal}, AS, AL, BL);

eval_target(self, AS, _AL, _BL) ->
    [AS];
eval_target(enemy, _AS, _AL, BL) ->
    BL;
eval_target({friend, Num, Fun}, AS, AL, _BL) ->
    select_target(Fun, Num, AS, AL);
eval_target({enemy, Num, Fun}, AS, _AL, BL) ->
    select_target(Fun, Num, AS, BL).

% {friend,1,hp_live_min}
% {friend,all}
% {friend,all,died}
% {enemy,1}
% self
% {enemy,all,col}
% {enemy,1}
% enemy
% {enemy,1,behind}
% {enemy,1}
% enemy
% {enemy,all}
% {enemy,1}
% self
% {enemy,all,front_row}
% {enemy,all,front_row}
% self
% {enemy,3,rand}
% {enemy,3,rand}
% enemy
% self
% {enemy,1}
% {enemy,all}
% enemy
% {friend,2,rand}
% enemy
%
% {friend,1,hp_live_min}
% self
% {friend,1,hp_min}
% {friend,1,hp_live_min}
% 
% {friend,1,died}

%% 选择目标

select_target(normal, no_self, AS, Ss) ->
    lists:keydelete(AS#s.pos, #s.pos, Ss);

select_target(normal, Num, AS, Ss) ->
    Num1 = if 
               Num == all -> 9;
              true -> Num
           end,
    Poses = normal_pos(AS#s.pos),
    select_target_by_pos(Poses, Num1, Ss, []);

select_target(died, Num, _AS, Ss) ->
    Ss1 = [A || A <- Ss, A#s.hp == 0],
    if 
        Num == all -> Ss1;
        true -> lists:sublist(shuffle(Ss1), Num)
    end;

select_target(rand, Num, _AS, Ss) ->
    Ss1 = [S || S <- Ss, S#s.hp > 0],
    lists:sublist(shuffle(Ss1), Num);

select_target(col, Num, AS, Ss) ->
    Num1 = if 
               Num == all -> 3;
               true -> Num
           end,
    Poses = col_pos(AS#s.pos),
    select_target_by_col(Poses, Num1, Ss);

select_target(behind, Num, AS, Ss) ->
    Poses = behind_pos(AS#s.pos),
    select_target_by_pos(Poses, Num, Ss, []);

select_target(hp_live_min, Num, _AS, Ss) ->
    Ss1 = [A || A <- Ss, A#s.hp > 0],
    Ss2 = hp_ratio_asc(Ss1),
    lists:sublist(Ss2, Num);

select_target(hp_min, Num, _AS, Ss) ->
    Ss1 = hp_ratio_asc(Ss),
    lists:sublist(Ss1, Num);

select_target(front_row, Num, _AS, Ss) ->
    Num1 = if 
               Num == all -> 3;
               true -> Num
           end,
    Poses = front_row_pos(),
    select_target_by_col(Poses, Num1, Ss);
select_target(Fun, _Num, _AS, _Ss) ->
    ?WARN("undefined fun:~w", [Fun]),
    [].

eval_cmd(Cmd, A, Targets) ->
    eval_cmd(Cmd, ok, A, Targets).

eval_cmd([{Fun, Args} | Cmds], ExtArg, A, Targets) ->
    Targets1 = mod_battle_fun:Fun(Targets, A, Args, ExtArg, []),
    eval_cmd(Cmds, ExtArg, A, Targets1);
eval_cmd([], _, _, Result) -> 
    Result;
eval_cmd(undefined, _, _, Result) -> 
    Result.

hp_ratio_asc(Ss) -> 
    Ss1 = [S#s{hp_ratio = Hp / Max} || S = #s{hp = Hp, hp_max = Max} <- Ss],
    hp_ratio_asc1(Ss1).

hp_ratio_asc1([]) -> [];
hp_ratio_asc1([H0 | T]) ->
    hp_ratio_asc1([H || H <- T, H#s.hp_ratio < H0#s.hp_ratio])
    ++ [H0] ++
    hp_ratio_asc1([H || H <- T, H#s.hp_ratio >= H0#s.hp_ratio]).

%% sort_s([], _) -> [];
%% sort_s([H0 | T], hp_min) ->
%%     sort_s([H || H <- T, H#s.hp < H0#s.hp], hp_min)
%%     ++ [H0] ++
%%     sort_s([H || H <- T, H#s.hp >= H0#s.hp], hp_min);
%% sort_s([H0 | T], hp_max) ->
%%     sort_s([H || H <- T, H#s.hp > H0#s.hp], hp_max)
%%     ++ [H0] ++
%%     sort_s([H || H <- T, H#s.hp =< H0#s.hp], hp_max).


%%' 修正为非负数
min0(A) when A >= 0 -> A;
min0(_) -> 0.
%%.

%% set_status(Bout, AL, BL) ->
%%     case chk_bout_over(AL, BL) of
%%         true ->
%%             AL1 = [A#s{status = 0} || A <- AL],
%%             BL1 = [B#s{status = 0} || B <- BL],
%%             {Bout + 1, AL1, BL1};
%%         false -> 
%%             {Bout, AL, BL}
%%     end.
%% 
%% chk_bout_over(AL, BL) ->
%%     chk_bout_over(AL) andalso chk_bout_over(BL).
%% 
%% chk_bout_over([#s{hp = H, status = S} | Ss]) ->
%%     case H > 0 andalso S =:= 0 of
%%         true -> false;
%%         false -> chk_bout_over(Ss)
%%     end;
%% chk_bout_over([]) -> true.

%% fix_status(X) when X > 0 -> X - 1;
%% fix_status(0) -> 
%%     ?WARN("fix_status", []),
%%     0.

get_s(Pos, Ss) ->
    lists:keyfind(Pos, #s.pos, Ss).

%% () -> {AL2, BL2}
set_ssss(Targets, AL, BL) ->
    [#s{pos=AP}|_] = AL,
    AP1 = AP div 10,
    %% lists:foldl(fun(X, Sum) -> X + Sum end, 0, [1,2,3,4,5]).
    {AT, BT} = lists:foldl(fun(T, {InA, InB}) ->
                                   case (T#s.pos div 10) == AP1 of
                                       true -> {[T|InA], InB};
                                       false -> {InA, [T|InB]}
                                   end
                           end, {[], []}, Targets),
    %% end, {[], []}, lists:merge(Targets)),
    AL2 = set_ss(AT, AL),
    BL2 = set_ss(BT, BL),
    {AL2, BL2}.

set_ss([S | T], Ss) ->
    Ss1 = set_s(S, Ss),
    set_ss(T, Ss1);
set_ss([], Ss) -> 
    Ss.

set_s(S, Ss) ->
    lists:keyreplace(S#s.pos, #s.pos, Ss, S).

set_used_wake_skill(wake, Pos, Ss) ->
    S1 = get_s(Pos, Ss),
    %% S2 = S1#s{enable_wake_skill = false, status = 1},
    S2 = S1#s{enable_wake_skill = false},
    lists:keyreplace(S2#s.pos, #s.pos, Ss, S2);
set_used_wake_skill(_, Pos, Ss) -> 
    S1 = get_s(Pos, Ss),
    %% S2 = S1#s{status = 1},
    S2 = S1,
    lists:keyreplace(S2#s.pos, #s.pos, Ss, S2).

%% 选择觉醒技能
-spec select_skill(Active, Pos, AL, BL) ->
    {false, NewAL} | {Targets, Skill, SkillType} when
    Active :: true | false,
    AL :: BL,
    BL :: NewAL,
    NewAL :: [#s{}],
    Targets :: [#s{}],
    Skill :: list(),
    Pos :: integer(),
    SkillType :: wake | active | normal.

select_skill(Active, Pos, AL, BL) ->
    S = get_s(Pos, AL),
    S1 = do_buff(disable_skill, S),
    if
        S1 =/= false -> 
            AL1 = set_s(S1, AL),
            {false, AL1};
        S#s.enable_wake_skill == false ->
            %% 已经放过觉醒技
            select_active_skill(Active, S, AL, BL);
        true ->
            case get_skill_by_trigger(S#s.skills, S, wake) of
                false -> select_active_skill(Active, S, AL, BL);
                Skill -> 
                    TargetCmd = util:get_val(target, Skill),
                    case eval_target(TargetCmd, S, AL, BL) of
                        [] -> select_active_skill(Active, S, AL, BL);
                        Targets -> {Targets, Skill,  wake}
                    end
            end
    end.

%% 非技能回合
select_active_skill(false, S, AL, BL) when S#s.job == 4 ->
    %% 治疗量计算=攻击方攻击*伤害波动*暴击指数
    HpAdd = S#s.atk * calc_dmg_offset() * calc_crit_num(S),
    %% ?INFO("cure hp: ~w", [HpAdd]),
    Skill = [{cmd, [{add_hp,{'+',HpAdd}}]}],
    Targets = eval_target({friend,1,hp_live_min}, S, AL, BL),
    Targets1 = mod_battle_fun:set_states(Targets, ?STA_CURE),
    {Targets1, Skill,  normal};
select_active_skill(false, _S, AL, _) ->
    {false, AL};

select_active_skill(true, S, AL, BL) ->
    case get_skill_by_trigger(S#s.skills, S, active) of
        false -> {false, AL};
        Skill -> 
            TargetCmd = util:get_val(target, Skill),
            case eval_target(TargetCmd, S, AL, BL) of
                [] -> {false, AL};
                Targets -> {Targets, Skill, active}
            end
    end.

select_atk_skill(A, B) ->
    case get_skill_by_trigger(A#s.skills, {A, B}, atk) of
        false -> {A, B};
        Skill ->
            Cmd = util:get_val(cmd, Skill),
            case util:get_val(target, Skill) of
                self -> 
                    [A1] = eval_cmd(Cmd, A, [A]),
                    {A1, B};
                enemy -> 
                    [B1] = eval_cmd(Cmd, A, [B]),
                    {A, B1}
            end
    end.

select_atked_skill([Skill | Skills], Atked, A, B) ->
    case util:get_val(trigger, Skill) of
        {atked, Arg} -> 
            case eval_trigger_arg(Arg, {A, B}) of
                false -> 
                    select_atked_skill(Skills, Atked, A, B);
                true -> 
                    Cmd = util:get_val(cmd, Skill),
                    %% ?INFO("[Atked:~w, Cmd:~w]", [Atked, Cmd]),
                    case util:get_val(target, Skill) of
                        self -> 
                            %% ?INFO("[Cmd:~w, Atked:~w, A:~w]", [Cmd, Atked, A]),
                            [A1] = eval_cmd(Cmd, Atked, A, [A]),
                            select_atked_skill(Skills, Atked, A1, B);
                        enemy -> 
                            [B1] = eval_cmd(Cmd, Atked, A, [B]),
                            select_atked_skill(Skills, Atked, A, B1);
                        Target ->
                            ?WARN("undef target:~w", [Target]),
                            select_atked_skill(Skills, Atked, A, B)
                    end
            end;
        _ -> select_atked_skill(Skills, Atked, A, B)
    end;
select_atked_skill([], _Atked, A, B) -> {A, B}.

select_skill_atked_skill([Skill | Skills], Atked, A, B) ->
    case util:get_val(trigger, Skill) of
        {skill_atked, Arg} -> 
            case eval_trigger_arg(Arg, {A, B}) of
                false -> 
                    select_skill_atked_skill(Skills, Atked, A, B);
                true -> 
                    Cmd = util:get_val(cmd, Skill),
                    %% ?INFO("[Atked:~w, Cmd:~w]", [Atked, Cmd]),
                    case util:get_val(target, Skill) of
                        self -> 
                            %% ?INFO("[Cmd:~w, Atked:~w, A:~w]", [Cmd, Atked, A]),
                            [A1] = eval_cmd(Cmd, Atked, A, [A]),
                            select_skill_atked_skill(Skills, Atked, A1, B);
                        enemy -> 
                            [B1] = eval_cmd(Cmd, Atked, A, [B]),
                            select_skill_atked_skill(Skills, Atked, A, B1);
                        Target ->
                            ?WARN("undef target:~w", [Target]),
                            select_skill_atked_skill(Skills, Atked, A, B)
                    end
            end;
        _ -> select_skill_atked_skill(Skills, Atked, A, B)
    end;
select_skill_atked_skill([], _Atked, A, B) -> {A, B}.

select_die_skill([Skill | Skills], Atked, A0, B) ->
    case util:get_val(trigger, Skill) of
        {die, Arg} -> 
            A = case Arg of
                    wake -> 
                        %% ?INFO("wake1:~w", [A0#s.skills]),
                        %% ?INFO("wake2:~w", [lists:delete(Skill, A0#s.skills)]),
                        A0#s{skills = lists:delete(Skill, A0#s.skills)};
                    _ -> A0
                end,
            case eval_trigger_arg(Arg, {A, B}) of
                false -> 
                    select_die_skill(Skills, Atked, A, B);
                true -> 
                    Cmd = util:get_val(cmd, Skill),
                    %% ?INFO("[die Cmd:~w]", [Cmd]),
                    case util:get_val(target, Skill) of
                        self -> 
                            %% ?INFO("[Cmd:~w, Atked:~w, A:~w]", [Cmd, Atked, A]),
                            [A1] = eval_cmd(Cmd, Atked, A, [A]),
                            select_die_skill(Skills, Atked, A1, B);
                        enemy -> 
                            [B1] = eval_cmd(Cmd, Atked, A, [B]),
                            select_die_skill(Skills, Atked, A, B1);
                        Target ->
                            ?WARN("undef target:~w, ~nSkill:~w", [Target, Skill]),
                            select_die_skill(Skills, Atked, A, B)
                    end
            end;
        _ -> select_die_skill(Skills, Atked, A0, B)
    end;
select_die_skill([], _Atked, A, B) -> {A, B}.

select_init_skill([S | Ss], AL, BL) ->
    case get_skill_by_trigger(S#s.skills, S, init) of
        false -> 
            select_init_skill(Ss, AL, BL);
        Skill ->
            TargetCmd = util:get_val(target, Skill), 
            Cmd = util:get_val(cmd, Skill),
            case eval_target(TargetCmd, S, AL, BL) of
                [] -> 
                    select_init_skill(Ss, AL, BL);
                Targets -> 
                    Targets1 = eval_cmd(Cmd, S, Targets),
                    {AL1, BL1} = set_ssss(Targets1, AL, BL),
                    select_init_skill(Ss, AL1, BL1)
            end
    end;
select_init_skill([], AL, BL) -> {AL, BL}.


%% 选择攻击者
-spec select_atk(CurPos, AL) -> {ok, NewAL, Pos} | {next, NewAL} when
    CurPos :: integer(),
    AL :: NewAL,
    NewAL :: [#s{}],
    Pos :: integer().

select_atk(CurPos, AL) ->
    %% AL1 = [A || A <- AL, A#s.status =:= 0, A#s.hp > 0],
    %% case AL1 of
    %%     [] -> {next, AL};
    %%     _ ->
    %%         %% 从存活的英雄中选择一个
    %%         [H | _] = sort_pos(AL1),
    %%         case do_buffs([dizzy, ice, horror], false, H) of
    %%             false -> 
    %%                 %% 初始化表现状态
    %%                 AL2 = set_s(H, AL),
    %%                 {ok, AL2, H#s.pos};
    %%             H1 ->
    %%                 AL2 = set_s(H1, AL),
    %%                 {next, AL2}
    %%         end
    %% end.
    AL1 = [A || A <- AL, A#s.hp > 0],
    case get_s(CurPos, AL1) of
        false -> {next, AL};
        H ->
            case do_buffs([dizzy, ice, horror], false, H) of
                false -> 
                    %% 初始化表现状态
                    AL2 = set_s(H, AL),
                    {ok, AL2, H#s.pos};
                H1 ->
                    AL2 = set_s(H1, AL),
                    {next, AL2}
            end
    end.

-spec do_buffs([BuffName], Status, Arg) -> false | NewArg when
    BuffName :: atom(),
    Status :: boolean(),
    Arg :: NewArg,
    NewArg :: term().

do_buffs([Name | L], Status, Arg) ->
    case do_buff(Name, Arg) of
        false -> do_buffs(L, Status, Arg);
        Arg1 -> do_buffs(L, true, Arg1)
    end;
do_buffs([], false, _Arg) -> false;
do_buffs([], true, Arg) -> Arg.

%% BUFF名称：
%%     dizzy         : 炫晕
%%     ice           : 冰冻
%%     horror        : 恐惧
%%     disable_skill : 禁止施法

-spec do_buff(BuffName, S) -> false | NewS when
    BuffName :: atom(),
    S :: NewS,
    NewS :: #s{}.

do_buff(disable_skill, S) ->
    Buffs = S#s.buffs,
    case get_buff(disable_skill, Buffs) of
        false -> false;
        Buff -> 
            sub_buff(Buff, S)
    end;
do_buff(dizzy, S) ->
    Buffs = S#s.buffs,
    case get_buff(dizzy, Buffs) of
        false -> false;
        Buff -> 
            S1 = sub_buff(Buff, S),
            %% S1#s{status = 1}
            S1
    end;
do_buff(ice, S) ->
    Buffs = S#s.buffs,
    case get_buff(ice, Buffs) of
        false -> false;
        Buff -> 
            S1 = sub_buff(Buff, S),
            %% S1#s{status = 1}
            S1
    end;
do_buff(horror, S) ->
    Buffs = S#s.buffs,
    case get_buff(horror, Buffs) of
        false -> false;
        Buff -> 
            S1 = sub_buff(Buff, S),
            %% S1#s{status = 1}
            S1
    end.

do_atk_buff([Buff | Buffs], A, A0, Nth) when Buff#buff.trigger == atk ->
    Name = Buff#buff.name,
    [A01] = mod_battle_fun:Name([A0], A, Buff#buff.args, ok, []),
    A1 = case Nth > 0 of
             true -> A;
             false -> sub_buff(Buff, A)
         end,
    do_atk_buff(Buffs, A1, A01, Nth);
do_atk_buff([_ | Buffs], A, A0, Nth) ->
    do_atk_buff(Buffs, A, A0, Nth);
do_atk_buff([], A, A0, _) -> {A, A0}.

%%' Select targets by pos
%%  不包括血量为0的英雄
select_target_by_pos([], _Num, _Ss, Result) -> Result;
select_target_by_pos([Pos | PosList], Num, Ss, Result) ->
    if 
        Num =< 0 -> 
            Result;
        Ss == [] ->
            Result;
        true ->
            [H|_] = Ss,
            Pos1 = Pos + H#s.pos div 10 * 10,
            case lists:keyfind(Pos1, #s.pos, Ss) of
                false -> select_target_by_pos(PosList, Num, Ss, Result);
                S when S#s.hp > 0 -> 
                    select_target_by_pos(PosList, Num-1, Ss, [S|Result]);
                _S ->
                    select_target_by_pos(PosList, Num, Ss, Result)
            end
    end.

select_target_by_col([PosL | PosLs], Num, Ss) ->
    case select_target_by_pos(PosL, Num, Ss, []) of
        [] -> select_target_by_col(PosLs, Num, Ss);
        Rt -> Rt
    end.
%%.

%%' pos list ...
%% Vim Command:
%% '<,'>s/\([1-9]\)\([1-9]\)/2\2/g

%% normal_pos(1) -> [1, 2, 3, 4, 5, 6, 7, 8, 9];
%% normal_pos(4) -> [1, 2, 3, 4, 5, 6, 7, 8, 9];
%% normal_pos(7) -> [1, 2, 3, 4, 5, 6, 7, 8, 9];
%% 
%% normal_pos(2) -> [2, 1, 3, 5, 4, 6, 8, 7, 9];
%% normal_pos(5) -> [2, 1, 3, 5, 4, 6, 8, 7, 9];
%% normal_pos(8) -> [2, 1, 3, 5, 4, 6, 8, 7, 9];
%% 
%% normal_pos(3) -> [3, 2, 1, 6, 5, 4, 9, 8, 7];
%% normal_pos(6) -> [3, 2, 1, 6, 5, 4, 9, 8, 7];
%% normal_pos(9) -> [3, 2, 1, 6, 5, 4, 9, 8, 7];

normal_pos(P) when P > 10 -> normal_pos(P rem 10);
normal_pos(1) -> [1, 2, 3, 4, 5, 6, 7, 8, 9];
normal_pos(4) -> [1, 2, 3, 4, 5, 6, 7, 8, 9];
normal_pos(7) -> [1, 2, 3, 4, 5, 6, 7, 8, 9];
normal_pos(2) -> [2, 1, 3, 5, 4, 6, 8, 7, 9];
normal_pos(5) -> [2, 1, 3, 5, 4, 6, 8, 7, 9];
normal_pos(8) -> [2, 1, 3, 5, 4, 6, 8, 7, 9];
normal_pos(3) -> [3, 2, 1, 6, 5, 4, 9, 8, 7];
normal_pos(6) -> [3, 2, 1, 6, 5, 4, 9, 8, 7];
normal_pos(9) -> [3, 2, 1, 6, 5, 4, 9, 8, 7].

%% 前排
front_row_pos() -> [[1, 2, 3], [4, 5, 6], [7, 8, 9]].

%% 跟随点
%% follow_pos(P) when P > 10 -> follow_pos(P div 10);
%% follow_pos(1) -> [4, 7];
%% follow_pos(2) -> [5, 8];
%% follow_pos(3) -> [6, 9];
%% follow_pos(4) -> [7];
%% follow_pos(5) -> [8];
%% follow_pos(6) -> [9];
%% follow_pos(_) -> [].

%% col_pos
%% '<,'>s/\([1-9]\)\([1-9]\)/2\2/g
col_pos(P) when P > 10 -> col_pos(P rem 10);
col_pos(1) -> [[1,4,7], [2,5,8], [3,6,9]];
col_pos(2) -> [[2,5,8], [1,4,7], [3,6,9]];
col_pos(3) -> [[3,6,9], [2,5,8], [1,4,7]];
col_pos(4) -> [[1,4,7], [2,5,8], [3,6,9]];
col_pos(5) -> [[2,5,8], [1,4,7], [3,6,9]];
col_pos(6) -> [[3,6,9], [2,5,8], [1,4,7]];
col_pos(7) -> [[1,4,7], [2,5,8], [3,6,9]];
col_pos(8) -> [[2,5,8], [1,4,7], [3,6,9]];
col_pos(9) -> [[3,6,9], [2,5,8], [1,4,7]].

behind_pos(P) when P > 10 -> behind_pos(P rem 10);
behind_pos(1) -> [7, 8, 9, 4, 5, 6, 1, 2, 3];
behind_pos(4) -> [7, 8, 9, 4, 5, 6, 1, 2, 3];
behind_pos(7) -> [7, 8, 9, 4, 5, 6, 1, 2, 3];
behind_pos(2) -> [8, 7, 9, 5, 4, 6, 2, 1, 3];
behind_pos(5) -> [8, 7, 9, 5, 4, 6, 2, 1, 3];
behind_pos(8) -> [8, 7, 9, 5, 4, 6, 2, 1, 3];
behind_pos(3) -> [9, 8, 7, 6, 5, 4, 3, 2, 1];
behind_pos(6) -> [9, 8, 7, 6, 5, 4, 3, 2, 1];
behind_pos(9) -> [9, 8, 7, 6, 5, 4, 3, 2, 1].
%%.

%%' sort_pos
%% sort_pos([]) -> [];
%% sort_pos([H0 | T]) ->
%%     sort_pos([H || H <- T, H#s.pos < H0#s.pos])
%%     ++ [H0] ++
%%     sort_pos([H || H <- T, H#s.pos >= H0#s.pos]).
%%.

%%' 检查是否结束
%% () -> 0|1|2|3
%% 0=未结束
%% 1=win
%% 2=lost
%% 3=平局
is_over(Index, Bout, AL0, BL0) ->
    L = AL0 ++ BL0,
    L1 = [S || S <- L, S#s.hp > 0],
    Data = lists:foldl(fun(E, {A, B}) -> 
                               case E#s.pos > 20 of
                                   true -> {A, [E|B]};
                                   false -> {[E|A], B}
                               end
                       end, {[], []}, L1),
    case Data of
        {[], _} -> 2;
        {_, []} -> 1;
        _ -> 
            case Bout > 25 orelse Index > 300 of
                %% true -> force_over(Data);
                true -> 2;
                false -> 0
            end
    end.

%% force_over({AL, BL}) ->
%%     A = lists:foldl(fun(S, Sum) -> S#s.atked + Sum end, 0, AL),
%%     B = lists:foldl(fun(S, Sum) -> S#s.atked + Sum end, 0, BL),
%%     if 
%%         A > B -> 2;
%%         A < B -> 1;
%%         true -> 3
%%     end.
%%.

-define(hero_format, 
        "| ~2w"    %% pos       
        " |~5w"      %% hp
        " |~5w"      %% atk
        " |~5w"      %% def
        " |~5w"      %% pun
        " |~5w"      %% hit
        " |~5w"      %% dod
        " |~5w"      %% crit
        " |~5w"      %% cnum
        " |~5w"      %% canti
        " |~5w"      %% tou
        " |~5w"      %% tanit
        " |~11w"     %% skill
        " |~5w"      %% id
        " |~5w"      %% tid
        " | ~1w"      %% job
        " |~2w"      %% sort
        " | ~1w"      %% rare
        " | ~1w"      %% changed
        " |~5w"      %% exp
        " |~2w"      %% lev
        " | ~1w |~n"   %% quality.
       ).

-define(hero_title_arg, 
    [
       ps
      ,hp
      ,atk
      ,def
      ,pun
      ,hit
      ,dod
      ,crit
      ,cnum
      ,canti
      ,tou
      ,tanit
      ,skill
      ,id
      ,tid
      ,j
      ,s
      ,r
      ,c
      ,exp
      ,lv
      ,q
     ]).

print_hero(Heroes) ->
    case env:get(debug) of
        on ->
            io:format(?hero_format, ?hero_title_arg),
            io:format("~s~n", [string:copies("-", 141)]),
            print_hero1(Heroes);
        _ -> ok
    end.

print_hero1([Hero | Heroes]) ->
    io:format(?hero_format, [
        Hero#hero.pos
        ,Hero#hero.hp
        ,Hero#hero.atk
        ,Hero#hero.def
        ,Hero#hero.pun
        ,Hero#hero.hit
        ,Hero#hero.dod
        ,Hero#hero.crit
        ,Hero#hero.crit_num
        ,Hero#hero.crit_anti
        ,Hero#hero.tou
        ,Hero#hero.tou_anit
        ,Hero#hero.skills
        ,Hero#hero.id
        ,Hero#hero.tid
        ,Hero#hero.job
        ,Hero#hero.sort
        ,Hero#hero.rare
        ,Hero#hero.changed
        ,Hero#hero.exp
        ,Hero#hero.lev
        ,Hero#hero.quality
       ]),
    print_hero1(Heroes);
print_hero1([]) -> 
    io:format("~s~n", [string:copies("-", 141)]),
    io:format(?hero_format, ?hero_title_arg),
    ok.

%%' print_battle
print_battle({IsOver, Data}) ->
    case env:get(debug) of
        on ->
            OverResult = case IsOver of
                1 -> "win";
                2 -> "lost";
                3 -> "***"
            end,
            TitleF = "~n~3s [YJUD] SKILL  TG  ID    HP [BSZ] [YJUD]~n",
            TitleV = ["ATk"],
            io:format(TitleF, TitleV),
            io:format("~s~n", [string:copies("-", 50)]),
            F1 = fun([AtkPos, AtkBuff, Targets, SkillId]) ->
                    BuffA2 = [integer_to_list(X) || X <- AtkBuff],
                    io:format("@~w [~4s] ~5w ", [AtkPos, BuffA2, SkillId]),
                    lists:foldl(fun([AtkedPos, Hp, Sta1, AtkedBuff], I) ->
                                Sta2 = [
                                    {"B" , Sta1 band ?STA_CRIT}
                                    ,{"S", Sta1 band ?STA_DOD}
                                    ,{"Z", Sta1 band ?STA_CURE}
                                    ,{"R", Sta1 band ?STA_REALIVE}
                                ],
                                Sta3 = [X1 || {X1, X2} <- Sta2, X2 > 0],
                                Buff3 = [integer_to_list(X) || X <- AtkedBuff],
                                case I of
                                    0 -> io:format(" -> ", []);
                                    _ -> io:format("~n~21s", [" "])
                                end,
                                io:format("@~w ~5w [~3s] [~4s]", [AtkedPos, Hp, Sta3, Buff3]),
                                I + 1
                        end, 0, Targets),
                    io:format("~n")
            end,
            lists:foreach(F1, Data),
            io:format("~s", [string:copies("-", 50)]),
            io:format(TitleF, TitleV),
            io:format("===~s===", [OverResult]),
            ok;
        _ -> ok
    end.
%%.

%% 命中判定指数：即是否命中计算，若命中，则值为1，未命中，则值为0
%% 命中率计算：
%% 当我的命中大于敌人闪避，则命中指数=1
%% 当我的命中小于敌人闪避，则命中指数=1+0.5*（10-6 *X*X-2*10-3*X）
%% 注：X=MIN(敌人的闪避-我的命中*1.2,1000)
%% 计算命中
calc_hit(A, B) ->
    case A#s.hit > B#s.dod of
        true -> 1;
        false ->
           X = min(B#s.dod - A#s.hit * 1.2, 1000), X1 = 1+0.5*(math:pow(10, -6)*X*X-2*math:pow(10, -3)*X),
           %% ?INFO("Hit:~w, Dod:~w, X:~w, X1:~w", [A#s.hit, B#s.dod, X, X1]),
           case rate(X1) of
               true -> 1;
               false -> 0
           end
    end.


%% 防御指数：即扣除防御所吸收部分而后所造成的伤害系数
%% 当被攻击方的防御值+ max((被攻击方韧性-攻击方无视韧性),0)大于攻击方的穿刺值时：
%% 防御指数=1+0.5*（10-6 *X*X-2*10-3*X）
%% 注：公式中的X=MIN(被击方防御值+ max((被攻击方韧性-攻击方无视韧性),0)-攻击方穿透值,1000)（防御值的计算包含人物属性，装备，被动技能等，其他属性以此类推）
%% 当被攻击方的防御值+ max((被攻击方韧性-攻击方无视韧性),0)小于攻击方的穿刺值时：
%% 防御指数=1-（10-6 *X*X-2*10-3*X）
%% 注：X=MIN(攻击方穿透值-被击方防御值- max((被攻击方韧性-攻击方无视韧性),0),1000)
calc_def(A, B) ->
    case (B#s.def + max(B#s.tou - A#s.tou_anit, 0)) > A#s.pun of
        true ->
            X = min(B#s.def + max(B#s.tou - A#s.tou_anit, 0) - A#s.pun, 1000),
            1+0.5*(math:pow(10, -6)*X*X-2*math:pow(10, -3)*X);
        false ->
            X = min(A#s.pun - B#s.def - max(B#s.tou - A#s.tou_anit, 0), 1000),
            1-0.5*(math:pow(10, -6)*X*X-2*math:pow(10, -3)*X)
    end.


%% 暴击判定指数：即是否暴击计算，若暴击，则值为1，未命中，则值为0
%% 暴击率：若攻击方的暴击大于被攻击方的抗暴+max((被攻击方韧性-攻击方无视韧性),0)则
%% 暴击率=（2*10-3*X-10-6 *X*X）/2
%% 注释：X=min(攻击方的暴击-被攻击方的抗暴- max((被攻击方韧性-攻击方无视韧性),0),1000)
%% 暴击指数：即暴击强度的伤害放大倍数=1.5-4.5*（10-6 *X*X-2*10-3*X）
%% 注：公式中的X=MIN(攻击方暴强，1000)
calc_crit(A, B) ->
    case A#s.crit > B#s.crit_anti + max(B#s.tou - A#s.tou_anit, 0) of
        true ->
            X = min(A#s.crit - B#s.crit_anti - max(B#s.tou - A#s.tou_anit, 0), 1000),
            X1 = (2*10-3*X-10-6*X*X)/2,
            case rate(X1) of
                true -> calc_crit_num(A);
                false -> 1
            end;
        false ->
            1
    end.

calc_crit_num(A) ->
    Y = min(A#s.crit_num, 1000), 
    1.5-4.5*(math:pow(10, -6)*Y*Y-2*math:pow(10, -3)*Y).

%% 伤害波动指数
%% 伤害波动指数=0.9+0.2*(random（100）/100)
calc_dmg_offset() ->
    0.9+0.2*(util:rand(1,100)/100).

%% 伤害=MAX(命中判定指数*（攻击方攻击*min(max((被攻击方韧性-攻击方无视韧性),0),1000)/1000）
%%       *防御指数*暴击指数 *伤害波动*(技能伤害系数A*技能伤害系数B*….）,0)
%% 伤害=MAX(命中判定指数*
%%      （攻击方攻击*min(max((被攻击方韧性-攻击方无视韧性),0),1000)/1000）*防御指数*暴击指数 *伤害波动,0)
calc_atk(A, B, Crit, AtkArg) ->
    Def = calc_def(A, B),
    Offset = calc_dmg_offset(),
    Rt = util:ceil(max((A#s.atk * (1 - min(max(B#s.tou - A#s.tou_anit, 0), 1000) / 1000)) * Def * Crit * Offset * AtkArg, 0)),
    %% ?INFO("RT:~w, Def:~w, Crit:~w, Offset:~w, AtkArg:~w", [Rt, Def, Crit, Offset, AtkArg]),
    Rt.

rate(R) ->
    R1 = R * 100000,
    case util:rand(1, 100000) of
        N when N =< R1 -> true;
        _ -> false
    end.

shuffle(L) ->
    List1 = [{random:uniform(), X} || X <- L],
    List2 = lists:keysort(1, List1),
    [E || {_, E} <- List2].

% get_alive(L) ->
%     [B || B <-L, B#s.hp > 0].

%% vim: set filetype=erlang foldmarker=%%',%%. foldmethod=marker:
