%%----------------------------------------------------
%% 排行榜服务
%%
%% $Id$
%%
%% @author Rolong<rolong@vip.qq.com>
%%----------------------------------------------------
-module(mod_attain).
-export([
        attain_state/3
        ,attain_state2/3
		,attain_init/1
		,attain_today/0
		,attain_stores/1
		,combat_attain/2
		,hero_attain/3
    ]
).
-include("common.hrl").
-include("prop.hrl").
-include("equ.hrl").
-include("hero.hrl").


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(TEST).
data_attain_test() ->
	Ids = data_attain:get(ids),
	?assert(length(Ids) > 0),
	lists:foreach(fun(Id) ->
				Data = data_attain:get(Id),
				?assert(util:get_val(type, Data, 0) > 0),
				?assert(util:get_val(condition, Data, 0) > 0),
				?assert(util:get_val(tid, Data, 0) > 0)
				  end, Ids),
	ok.
-endif.


%% --- 私有函数 ---

%% 成就是否达到条件,更新完成状态,
attain_state(Type, Num, Rs) ->
	MyAttain = Rs#role.attain,
	attain_state_send(MyAttain, Type, Num, Rs).

attain_state_send([{Id, NextId, Type, Condition, State}|T], Type, Num, Rs) ->
	MyAttain = Rs#role.attain,
	Data1 = data_attain:get(Id),
	S1 = util:get_val(condition, Data1),
	case State < 2 of
		true ->
			case Condition >= S1 of
				true ->
					Lists1 = {Id,NextId, Type, Condition + Num, State},
					Lists2 = lists:keyreplace(Id, 1, MyAttain, Lists1),
					Rs1 = Rs#role{attain = Lists2},
					attain_state_send(T, Type, Num, Rs1);
				false ->
					case Condition + Num >= S1 of
						true ->
							Lists1 = {Id,NextId, Type, Condition + Num, 1},
							Lists2 = lists:keyreplace(Id, 1, MyAttain, Lists1),
							sender:pack_send(Rs#role.pid_sender, 24001, [Id]),
							Rs1 = Rs#role{attain = Lists2},
							attain_state_send(T, Type, Num, Rs1);
						false ->
							Lists1 = {Id,NextId, Type, Condition + Num, State},
							Lists2 = lists:keyreplace(Id, 1, MyAttain, Lists1),
							Rs1 = Rs#role{attain = Lists2},
							attain_state_send(T, Type, Num, Rs1)
					end
			end;
		false ->
			Lists1 = {Id,NextId, Type, Condition + Num, State},
			Lists2 = lists:keyreplace(Id, 1, MyAttain, Lists1),
			Rs1 = Rs#role{attain = Lists2},
			attain_state_send(T, Type, Num, Rs1)
	end;

attain_state_send([{_,_,_C,_,_}|T], Type, Num, Rs) ->
	attain_state_send(T, Type, Num, Rs);

attain_state_send([], _Type, _Num, Rs) -> Rs.

%% 英雄级别成就(统帅..)
attain_state2(Type, Num, Rs) ->
	MyAttain = Rs#role.attain,
	Lists = [{A,B,C,D,E} || {A,B,C,D,E} <- MyAttain, C =:= Type],
	attain_state_send2(Lists, Num, Rs).

attain_state_send2([T|H], Num, Rs) ->
	MyAttain = Rs#role.attain,
	{Id, NextId, Type, _Condition, State} = T,
	Data1 = data_attain:get(Id),
	S1 = util:get_val(condition, Data1),
	case State < 2 of
		true ->
			case Num >= S1 of
				true ->
					Lists1 = {Id,NextId, Type, Num, 1},
					Lists2 = lists:keyreplace(Id, 1, MyAttain, Lists1),
					sender:pack_send(Rs#role.pid_sender, 24001, [Id]),
					Rs1 = Rs#role{attain = Lists2},
					attain_state_send2(H, Num, Rs1);
				false ->
					Rs
			end;
		false ->
			Rs
	end;

attain_state_send2([], _Num, Rs) -> Rs.

%% 日常成就数据隔天重置
-spec attain_init(Rs) -> {ok, Rs} when
    Rs :: #role{}.
attain_init(Rs) ->
    MyId = Rs#role.id,
    Sql = list_to_binary([<<"SELECT `ctime`, `attain` FROM `attain` WHERE `id` = ">>, integer_to_list(MyId), <<";">>]),
    {Ctime, Attain} = case db:get_row(Sql) of
        {ok, [A,B]} -> {A, unzip(B)};
        {error, null} -> {0, []};
        {error, _Reason} -> {0, []}
    end,
    F = fun(Id,NextId,Type,Condition,State) ->
            Data = data_attain:get(Id),
            Title = util:get_val(title, Data),
            case Title >= 6 of
                true -> 0;
                false -> {Id, NextId, Type, Condition, State}
            end
    end,
    case Ctime =:= 0 of
        true -> {ok, Rs};
        false ->
            case Ctime =< util:unixtime(today) of
                true ->
                    NewAttain = [F(A,B,C,D,E) || {A,B,C,D,E} <- Attain, F(A,B,C,D,E) > 0],
                    TodayTask = attain_today(),
                    {ok, Rs#role{attain = NewAttain ++ TodayTask}};
                false ->
                    {ok, Rs#role{attain = Attain}}
            end
    end.


%% 日常成就抽取
attain_today() ->
	Ids = data_attain:get(ids),
	F1 = fun(Id) ->
				Data = data_attain:get(Id),
				Title = util:get_val(title, Data),
				case Title >= 6 of
					true -> {Id, Title};
					false -> 0
				end
		end,
	T = [F1(Id) || Id <- Ids, F1(Id) > 0],
	Sex = [Id || {Id, Title} <- T, Title =:= 6],
	Seven = [Id || {Id, Title} <- T, Title =:= 7],
	Eigth = [Id || {Id, Title} <- T, Title =:= 8],
	F2 = fun(Id) -> D1 = data_attain:get(Id),
					T1 = util:get_val(type, D1),
					{Id, 0, T1, 0, 0}
		 end,
	Lists1 = [F2(Id) || Id <- util:rand_element(2, Sex)],
	Lists2 = [F2(Id) || Id <- util:rand_element(2, Seven)],
	Lists3 = [F2(Id) || Id <- util:rand_element(1, Eigth)],
	Lists1 ++ Lists2 ++ Lists3.

%% 数据库相关操作

attain_stores(Rs) ->
    #role{
        id	=	Id
        ,attain = Attain
    } = Rs,
    Val = ?ESC_SINGLE_QUOTES(zip(Attain)),
    Ctime = util:unixtime(),
    Sql = list_to_binary([
            <<"SELECT count(*) FROM `attain` WHERE id = ">>,integer_to_list(Id),<<";">>]),
    case db:get_one(Sql) of
        0 ->
            Sql2 = list_to_binary([<<"INSERT `attain` (`id`, `ctime`, `attain`) VALUES (">>
                    , integer_to_list(Id), <<",">>, integer_to_list(Ctime), <<",'">>, Val, <<"');">>]),
            db:execute(Sql2);
        1 ->
            Sql1 = list_to_binary([<<"UPDATE `attain` SET `attain` = '">>
                    , Val,<<"',">>, <<"`ctime` = ">>, integer_to_list(Ctime)
                    , <<" WHERE `id` = ">>, integer_to_list(Id), <<";">>]),
            db:execute(Sql1);
        _ ->
            ok
    end.

%% zip / unzip
%% 解包binary
unzip(Bin) ->
    <<Version:8, Bin1/binary>> = Bin,
    case Version of
        1 ->
			unzip1(Bin1, []);
        _ ->
            ?ERR("undefined version: ~w", [Version]),
            undefined
    end.

unzip1(<<X1:32, X2:32, X3:32, X4:32, X5:32, RestBin/binary>>, Rt) ->
	Rt1 = [{X1, X2, X3, X4, X5} | Rt],
	unzip1(RestBin, Rt1);
unzip1(<<>>, Rt) ->
	Rt.

%% 打包成binary
-define(ATTAIN_ZIP_VERSION, 1).
zip(L) ->
	Bin = list_to_binary([<<X1:32, X2:32, X3:32, X4:32, X5:32>> || {X1, X2, X3, X4, X5} <- L]),
	<<?ATTAIN_ZIP_VERSION:8, Bin/binary>>.

%% 战斗成就推送
combat_attain(Id, Rs) ->
	if
		Id == 1301 ->
			attain_state(2, 1, Rs);
		Id == 2301 ->
			attain_state(3, 1, Rs);
		Id == 3301 ->
			attain_state(4, 1, Rs);
		Id == 4301 ->
			attain_state(5, 1, Rs);
		Id == 5301 ->
			attain_state(6, 1, Rs);
		Id == 6301 ->
			attain_state(7, 1, Rs);
		true -> Rs
	end.

%% 英雄成就推送
hero_attain(T, S, Rs) ->
	Data = Rs#role.heroes,
	F = fun(Hero) ->
				#hero{lev = Lev} = Hero,
				Lev
		end,
	Num1 = length([F(X) || X <- Data, F(X) >= S]),
	%% Num2 = length([F(X) || X <- Data, F(X) >= 30]),
	%% Num3 = length([F(X) || X <- Data, F(X) >= 45]),
	%% Num4 = length([F(X) || X <- Data, F(X) >= 60]),
	case Num1 =:= 0 of
		true -> Rs;
		false ->
			attain_state2(T, Num1, Rs)
	end.

%% vim: set ft=erlang foldmethod=marker foldmarker=%%',%%.:
