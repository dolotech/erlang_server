%%----------------------------------------------------
%% 协议22 - 战斗
%%
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-module(pt_combat).
-export([handle/3]).

-include("common.hrl").
-include("fb.hrl").

%% 主线战斗
handle(22002, [1, GateId, _], Rs) ->
    ?DEBUG("[Main line] GateId:~w, tollgate_id:~w", [GateId, Rs#role.tollgate_id]),
    Rs1 = lib_role:time2power(Rs),
    GateId1 = case GateId > 0 of
                  true -> GateId;
                  false -> Rs#role.tollgate_id
              end,
    {Data, Rs2} = mod_combat:combat(1, Rs1, GateId1),
    {ok, Data, Rs2};

%% 副本战斗
handle(22002, [2, GateId, _], Rs) ->
    ?DEBUG("[Fb] GateId:~w, fb_gate:~w", [GateId, Rs#role.fb_gate]),
    case Rs#role.fb_gate of
        {_, GateId} ->
            {Data, Rs2} = mod_combat:combat(2, Rs, GateId),
            {ok, Data, Rs2};
        false ->
            {ok, [0, 0, 0, [], []]}
    end;

%% 竞技场
handle(22002, [3, ArenaId, _], Rs) ->
    ?DEBUG("ArenaId:~w", [ArenaId]),
    {Data, Rs2} = mod_combat:arena(Rs, ArenaId),
    {ok, Data, Rs2};

handle(22010, [FbId, Type], Rs) ->
    {Rs1, Num, RestSec} = fb_refresh(Rs, Type),
    GateId = case Num > 0 of
        true -> get_fb_gate_id(FbId, Type);
        false -> 0
    end,
    {ok, [GateId, Num, RestSec], Rs1#role{fb_gate = {Type, GateId}}};

handle(_Cmd, _Data, _RoleState) ->
    {error, bad_request}.

%% === 私有函数 ===

get_fb_gate_id(FbId, Type) ->
    Data1 = data_fb_monsters:get({FbId, Type}),
    Data2 = data_fb_monsters:get({FbId, Type+1}),
    case Data1 =/= undefined andalso Data2 =/= undefined of
        true ->
            {GateId} = util:rand_element(Data1 ++ Data2),
            GateId;
        false ->
            ?WARN("Error FbId:~w, Type:~w", [FbId, Type]),
            0
    end.


%% -> {Rs, RestSec}
fb_refresh(Rs, 1) ->
    #role{fb_combat1 = N, fb_time1 = T} = Rs,
    Max = data_config:get(fb_max1),
    Cd = data_config:get(fb_cd1),
    {N1, T1, RestSec} = fb_refresh1(N, T, Cd, Max),
    Rs1 = Rs#role{fb_combat1 = N1, fb_time1 = T1},
	%% 成就推送(日常成就)
	Rs0 = mod_attain:attain_state(56, 1, Rs1),
    {Rs0, N1, RestSec};
fb_refresh(Rs, 2) ->
    #role{fb_combat2 = N, fb_time2 = T} = Rs,
    Max = data_config:get(fb_max2),
    Cd = data_config:get(fb_cd2),
    {N1, T1, RestSec} = fb_refresh1(N, T, Cd, Max),
    Rs1 = Rs#role{fb_combat2 = N1, fb_time2 = T1},
	Rs0 = mod_attain:attain_state(55, 1, Rs1),
    {Rs0, N1, RestSec};
fb_refresh(Rs, 3) ->
    #role{fb_combat3 = N, fb_time3 = T} = Rs,
    Max = data_config:get(fb_max3),
    Cd = data_config:get(fb_cd3),
    {N1, T1, RestSec} = fb_refresh1(N, T, Cd, Max),
    Rs1 = Rs#role{fb_combat3 = N1, fb_time3 = T1},
	Rs0 = mod_attain:attain_state(54, 1, Rs1),
    {Rs0, N1, RestSec};
fb_refresh(Rs, Type) ->
    ?WARN("Error Type: ~w", [Type]),
    {Rs, 0, 9999}.

%% -> {Num, Time, RestSec}
fb_refresh1(0, 0, Cd, Max) ->
    {Max, util:unixtime(), Cd};
fb_refresh1(Num, LastTime, Cd, Max) ->
    case Num >= Max of
        true ->
            {Max, util:unixtime(), Cd};
        false ->
            Now = util:unixtime(),
            Sec = Now - LastTime,
            case Sec >= Cd of
                true ->
                    RestSec = Sec rem Cd,
                    AddNum = Sec div Cd,
                    Num1 = Num + AddNum,
                    LastTime1 = Now - RestSec,
                    fb_refresh1(Num1, LastTime1, Cd, Max);
                false ->
                    {Num, LastTime, Cd - Sec}
            end
    end.
