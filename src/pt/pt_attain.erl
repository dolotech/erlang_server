%%----------------------------------------------------
%% 协议24 - 成就
%%
%% $Id$
%%
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-module(pt_attain).
-export([handle/3]).

-include("common.hrl").
-include("hero.hrl").
-include("equ.hrl").
-include("prop.hrl").

%% 领取成就
handle(24004, [Id], Rs) ->
	Data = data_attain:get(Id),
	PriceNum = util:get_val(num, Data),
	if
		Data == undefined ->
			{ok, [127, 0, 0]};
		true ->
			case util:get_val(tid, Data) of
				1 ->
					%% NewDiamond = Rs#role.diamond + PriceNum,
					%% Rs1 = Rs#role{diamond = NewDiamond},
					Rs1 = lib_role:add_attr(diamond, PriceNum, Rs),
					Rs2 = lib_role:add_attr_ok(diamond, 27, Rs, Rs1),
					lib_role:notice(Rs2),
					{NextId, State, Rs0} = attain_next(Id, Rs2),
					?DEBUG("NextId:~w, State:~w ", [NextId, State]),
					{ok, [0, NextId, State], Rs0};
				2 ->
					%% NewGold = Rs#role.gold + PriceNum,
					%% Rs1 = Rs#role{gold = NewGold},
					Rs1 = lib_role:add_attr(gold, PriceNum, Rs),
					Rs2 = lib_role:add_attr_ok(gold, 27, Rs, Rs1),
					lib_role:notice(Rs2),
					{NextId, State, Rs0} = attain_next(Id, Rs2),
					{ok, [0, NextId, State], Rs0};
				3 ->
					%% {Atar, A, B, C} = Rs#role.luck,
					%% Luck = {Atar + PriceNum, A, B, C},
					%% Rs1 = Rs#role{luck = Luck},
					Rs1 = lib_role:add_attr(luck, PriceNum, Rs),
					lib_role:notice(luck, Rs1),
					{NextId, State, Rs2} = attain_next(Id, Rs1),
					{ok, [0, NextId, State], Rs2};
				Tid ->
					case mod_item:add_item(Rs, Tid, PriceNum) of
						{ok, Rs1, PA, EA} ->
							mod_item:send_notice(Rs1#role.pid_sender, PA, EA),
							{NextId, State, Rs2} = attain_next(Id, Rs1),
							{ok, [0, NextId, State], Rs2#role{save = [role, items]}};
						%% TODO:邮件系统
						{error, full} ->
							{ok, [3, 0, 0]};
						{error, _} ->
							{ok, [128, 0, 0]}
					end
			end
	end;

%% 初始化成就
handle(24006, [], Rs) ->
	IdList = data_attain:get(ids),
	case Rs#role.attain of
		[] ->
			F1 = fun(Id) ->
						 Team = data_attain:get(Id),
						 Type = util:get_val(type, Team),
						 case util:get_val(start, Team, 0) of
							 1 ->
								 %% Condition = data_attain:get(condition, Team),
								 case util:get_val(next, Team, 0) of
									 0 -> {Id, 0, Type, 0, 0};
									 NextId ->
										 {Id, NextId, Type, 0, 0}
								 end;
							 0 -> 0
						 end
				 end,
			AttainList1 = [F1(Id) || Id <- IdList, F1(Id) > 0],
			?DEBUG("AttainList1:~w", [AttainList1]),
			L2 = mod_attain:attain_today() ++ AttainList1,
			?DEBUG("Attain_today:~w", [L2]),
			L = [{Id, State} || {Id, _, _, _, State} <- L2],
			?DEBUG("L:~w", [L]),
			Rs1 = Rs#role{attain = L2},
			{ok, [L], Rs1};
		Attain ->
			?DEBUG("Attain:~w", [Attain]),
			F2 = fun(Id, NextId, Type, Condition, State) ->
						 case State =:= 2 of
							 true ->
								 Team = data_attain:get(Id),
								 S = util:get_val(condition, Team),
								 case util:get_val(next, Team, 0) of
									 0 -> {Id, 0, Type, Condition, 2};
									 Nid ->
										 case Condition >= S of
											 true ->
												 {Id, Nid, Type, Condition, 1};
											 false ->
												 {Id, Nid, Type, Condition, 0}
										 end
								 end;
							 false ->
								 {Id, NextId, Type, Condition, State}
						 end
				 end,
			T = [F2(A,B,C,D,E) || {A,B,C,D,E} <- Attain],
			Rs1 = Rs#role{attain = T},
			L = [{Id, State} || {Id, _, _, _, State} <- T, State < 2],
			{ok, [L], Rs1}
	end;

handle(_Cmd, _Data, _RoleState) ->
    {error, bad_request}.


%% === 私有函数 ===


%% 下一个成就
attain_next(Id, Rs) ->
	Data = data_attain:get(Id),
	MyAttain = Rs#role.attain,
	Lists1 = lists:keydelete(Id, 1, MyAttain),
	%% 是否存在下一个成就
	case util:get_val(next, Data, 0) of
		0 ->
			{A, B, C, D, _E} = lists:keyfind(Id, 1, MyAttain),
			Lists2 = [{A,B,C,D,2} | Lists1],
			Rs1 = Rs#role{attain = Lists2},
			{0, 0, Rs1};
		NextId ->
			Data2 = data_attain:get(NextId),
			{_, _, Type, Condition, _} = lists:keyfind(Id, 1, MyAttain),
			S = util:get_val(condition, Data2),
			%% 是否已经完成可领取
			case Condition >= S of
				true ->
					Lists2 = [{NextId, 0, Type, Condition, 0} | Lists1],
					Rs1 = Rs#role{attain = Lists2},
					{NextId, 1, Rs1};
				false ->
					%% 是否存在下一个成就
					case util:get_val(next, Data2, 0) of
						0 ->
							Lists2 = [{NextId, 0, Type, Condition, 0} | Lists1],
							Rs1 = Rs#role{attain = Lists2},
							{NextId, 0, Rs1};
						NextId2 ->
							Lists2 = [{NextId, NextId2, Type, Condition, 0} | Lists1],
							Rs1 = Rs#role{attain = Lists2},
							{NextId, 0, Rs1}
					end
			end
	end.

%% %% 成就是否达到条件,更新完成状态,
%% attain_state(Type, Num, Rs) ->
%% 	MyAttain = Rs#role.attain,
%% 	case lists:keyfind(Type, 3, MyAttain) of
%% 		false -> Rs;
%% 		{Id, NextId, Type, Condition, State} ->
%% 			Data1 = data_attain:get(Id),
%% 			S1 = util:get_val(condition, Data1),
%% 			%% Data2 = data_attain:get(NextId),
%% 			%% S2 = util:get_val(condition, Data2),
%% 			case Condition >= S1 of
%% 				true ->
%% 					Lists1 = {Id,NextId, Type, Condition + Num, State},
%% 					Lists2 = lists:keyreplace(Id, 1, MyAttain, Lists1),
%% 					Rs#role{attain = Lists2};
%% 				false ->
%% 					case Condition + Num >= S1 of
%% 						true ->
%% 							Lists1 = {Id,NextId, Type, Condition + Num, 1},
%% 							Lists2 = lists:keyreplace(Id, 1, MyAttain, Lists1),
%% 							sender:pack_send(Rs#role.pid_sender, 24001, [Id]),
%% 							Rs#role{attain = Lists2};
%% 						false ->
%% 							Lists1 = {Id,NextId, Type, Condition + Num, State},
%% 							Lists2 = lists:keyreplace(Id, 1, MyAttain, Lists1),
%% 							Rs#role{attain = Lists2}
%% 					end
%% 			end
%% 	end.


%% vim: set ft=erlang foldmethod=marker foldmarker=%%',%%.:
