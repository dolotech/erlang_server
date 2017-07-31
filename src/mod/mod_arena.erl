%% ------------------------------------------------------------------
%% $Id: mod_arena.erl 4410 2013-11-08 09:09:11Z rolong $
%%
%% 竞技场
%% ------------------------------------------------------------------
-module(mod_arena).
-behaviour(gen_server).
-include("common.hrl").
-include("hero.hrl").
-include("offline.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-define(SERVER, arena).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
         start_link/0
         ,import_robot/0
         ,get_data/1
         ,get_team1/1
         ,get_team2/1
         ,fix_arena_pos/1
         ,zip_heroes/1
         ,unzip_heroes/1
         ,set_data/4
		 ,add_arena_exp/2
		 %% ,get_rank/0
		 %% ,update_rank_data/6
         ,get_rank_pos/2
         ,db_init/1
         ,db_update/1
         ,update_rank/0
		 ,update_arena_state/2
		 ,arena_init/1
		 ,arena_stores/1
		 ,zip/1
		 ,zip2/1
		 ,unzip/1
		 ,unzip2/1
		 ,db_init2/1
		]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% 获得排名位置（注意：当返回0时表示没有上榜）
-spec get_rank_pos(Rid, Lev) -> MyRank when
    Rid :: integer(),
    Lev :: integer(),
    MyRank :: integer().

get_rank_pos(Rid, Lev) ->
    gen_server:call(?SERVER, {get_rank_pos, Rid, Lev}).

db_init(Rs) ->
    Sql = "SELECT id, lev, exp, picture FROM `arena` WHERE robot = 0 and rid = ~s",
    case db:get_row(Sql, [Rs#role.id]) of
        {ok, [Id, Lev, Exp, Pic]} ->
            {ok, Rs#role{
                arena_id = Id
                ,arena_lev = Lev
                ,arena_exp = Exp
                ,arena_picture = Pic
                ,arena_rank = mod_arena:get_rank_pos(Rs#role.id, Lev)
            }};
        {error, null} -> 
            {ok, Rs};
        {error, Reason} -> 
            {error, Reason}
    end.

%% 登陆初始化竞技场信息
db_init2(Rs) ->
    Sql = "SELECT rival, report FROM `arena` WHERE robot = 0 and rid = ~s",
    case db:get_row(Sql, [Rs#role.id]) of
        {ok, [<<>>,<<>>]} -> {ok, Rs};
        {ok, [Rival, Report]} ->
			Rival2 = lists:reverse(unzip(Rival)),
			Report2 = lists:reverse(unzip2(Report)),
            {ok, Rs#role{
				arena		= Rival2
				,arena_lost_report = Report2
            }};
        {error, null} -> 
            {ok, Rs};
        {error, Reason} -> 
            {error, Reason}
    end.

db_update(Rs) ->
    #role{
        id = Id
        ,arena_lev = Lev
        ,arena_exp = Exp
    } = Rs,
    IdL = integer_to_list(Id),
    LevL = integer_to_list(Lev),
    ExpL = integer_to_list(Exp),
    Sql = list_to_binary([
            <<"update arena set lev = ">>, LevL
            ,<<", exp = ">>, ExpL
            ,<<" where robot = 0 and rid = ">>, IdL
        ]),
    db:execute(Sql).

get_data(Id) ->
    Key = {arena, Id},
    util:test1(),
    case cache:get(Key) of
        undefined ->
            util:test1(),
            Sql = <<"SELECT robot, rid, lev, name, s FROM `arena` WHERE id = ~s">>,
            Data = case db:get_row(Sql, [Id]) of
                {ok, [1, Rid, Lev, Name, S]} -> {1, Rid, Lev, Name, binary_to_term(S)};
                {ok, [0, Rid, Lev, Name, S]} -> {0, Rid, Lev, Name, unzip_heroes(S)};
                {error, Reason} -> {error, Reason}
            end,
            util:test2("get_arena_data"),
            cache:set(Key, Data),
            Data;
        Data ->
            util:test2("get_arena_cache"),
            Data
    end.

set_data(Rs, Lev, Exp, Heroes) ->
    gen_server:cast(cache, {save_arena_cache, Rs, Lev, Exp, Heroes}).

%% 更新战斗击败状态
-spec update_arena_state(Name, Rs) -> Rs1 when
      Name :: string(),
      Rs  :: #role{},
      Rs1 :: #role{}.
update_arena_state(Name, Rs) ->
    %% arena:[[Id, name, picture, IsBeat]], IsBeat:0=未击败, 1=已击败
    ArenaTuple = [list_to_tuple([A,B,C,D]) || [A,B,C,D] <- Rs#role.arena],
    %% RidTuple = lists:keyfind(Rid, 1, ArenaTuple),
    %% 这里要匹配找不到数据的情况
    case lists:keyfind(Name, 2, ArenaTuple) of
        false -> Rs;
        {Aid, Aname, Apicture, _} ->
            ArenaList = lists:keyreplace(Name, 2, ArenaTuple, {Aid, Aname, Apicture, 1}),
            Arena = [tuple_to_list({E,F,G,H}) || {E,F,G,H} <- ArenaList],
            Rs#role{arena = Arena}
    end.

%% 竞技场固定数据隔天重置
-spec arena_init(Rs) -> {ok, Rs} when
	  Rs :: #role{}.
arena_init(Rs) ->
	case Rs#role.arena_time =< util:unixtime(today) of
		true ->
            {ok, Rs#role{arena_wars = 20, arena_chance = 0, arena_rank_box = 0
                    ,arena_revenge = 0, arena_prize = 0}};
		false ->
            {ok, Rs}
	end.

add_arena_exp(AddExp, Rs) ->
	NewExp = Rs#role.arena_exp + AddExp,
	Lev = Rs#role.arena_lev,
	[{exp_max, Exp_max}] = data_arena_exp:get(Lev),
	case NewExp < 0 of
		true ->
			case Lev =:= 1 of
				true ->
					Rs1 = Rs#role{arena_exp = 0},
					{1, 0, Rs1};
				false ->
					Lev1 = Lev - 1,
					[{exp_max, Exp}] = data_arena_exp:get(Lev1),
					Exp1 = Exp + NewExp,
					Rs1 = Rs#role{arena_exp = Exp1, arena_lev = Lev1},
					{Lev1, Exp1, Rs1}
			end;
		false ->
			case NewExp >= Exp_max of
				true ->
					LevMax = length(data_arena_exp:get(ids)),
					case Lev =:= LevMax of
						true ->
							{Lev, 0, Rs};
						false ->
							Lev1 = Lev + 1,
							Exp1 = NewExp - Exp_max,
							Rs1 = Rs#role{arena_lev = Lev1, arena_exp = Exp1},
							{Lev1, Exp1, Rs1}
					end;
				false ->
					Rs1 = Rs#role{arena_exp = NewExp},
					{Lev, NewExp, Rs1}
			end
	end.

%% 获取排名列表
%% get_rank() ->
%%     case get(rank_data) of
%%         undefined -> [];
%%         RankList -> RankList
%%     end.
%%
%% %% 更新排行榜
%% update_rank_data(Id, Name, Picture, Exp, Honor, Lev) ->
%%     RankData = get(rank_data),
%%     R1 = case lists:keyfind(Id, 1, RankData) of
%%         false ->
%%             #role{
%%                 id			= Id
%% 				,name		= Name
%% 				,arena_picture = Picture
%% 				,arena_exp  = Exp
%% 				,arena_honor = Honor
%% 				,arena_lev	 = Lev
%%             };
%%         R ->
%%             R#role{
%%                 arena_exp = R#role.arena_exp + Exp
%% 				,arena_lev = Lev
%% 				,arena_honor = Honor
%%             }
%%     end,
%%     NewRankData = lists:keystore(Id, 1, RankData, R1),
%%     put(rank_data, NewRankData),
%%     ok.

get_team1(ArenaId) ->

    Sql = <<"SELECT lev, exp FROM `arena` WHERE id = ~s">>,
    case db:get_row(Sql, [ArenaId]) of
        {ok, [Lev, Exp]} ->
            Sql1 = list_to_binary([<<"SELECT id, name, picture FROM `arena` WHERE power > 0 and id <> ">>, integer_to_list(ArenaId),<<" and lev = ">>, integer_to_list(Lev), <<" and exp < ">>, integer_to_list(Exp), <<" order by exp desc limit 60;">>]),
            Sql2 = list_to_binary([<<"SELECT id, name, picture FROM `arena` WHERE power > 0 and id <> ">>, integer_to_list(ArenaId),<<" and lev = ">>, integer_to_list(Lev), <<" and exp > ">>, integer_to_list(Exp), <<" order by exp asc limit 60;">>]),
            Data1 = case db:get_all(Sql1) of
                {ok, D1} -> D1;
                {error, null} -> [];
                {error, Reason1} -> 
                    ?WARN("Error When Arena get_team1: ~w", [Reason1]),
                    []
            end,
            Data2 = case db:get_all(Sql2) of
                {ok, D2} -> D2;
                {error, null} -> [];
                {error, Reason2} -> 
                    ?WARN("Error When Arena get_team1: ~w", [Reason2]),
                    []
            end,
            Data = Data1 ++ Data2,
            util:rand_element1(4, Data);
        {error, Reason} ->
            ?WARN("Error When Arena get_team1: ~w", [Reason]),
            []
    end.

get_team2(ArenaId) ->
    Sql = <<"SELECT lev FROM `arena` WHERE id = ~s">>,
    case db:get_one(Sql, [ArenaId]) of
        {ok, Lev0} ->
            Lev = max(1, Lev0 - 1),
            Sql1 = list_to_binary([<<"SELECT id, name, picture FROM `arena` WHERE id <> ">>, integer_to_list(ArenaId), <<" and power > 0 and lev = ">>, integer_to_list(Lev), <<" order by exp desc limit 60;">>]),
            case db:get_all(Sql1) of
                {ok, Data} -> util:rand_element1(4, Data);
                {error, Reason} ->
                    ?WARN("Error When Arena get_team2: ~w", [Reason]),
                    []
            end;
        {error, Reason} ->
            ?WARN("Error When Arena get_team2: ~w", [Reason]),
            []
    end.

import_robot() ->
    Ids = data_robot:get(ids),
    import_robot(Ids).

%% get(4) -> [{lev, 1}, {exp, 45}, {monster, [{3,2},{4,5}]}];
import_robot([Id | Ids]) ->
    Data = data_robot:get(Id),
    Lev = util:get_val(lev, Data),
    Exp = util:get_val(exp, Data),
    Pic = util:get_val(picture, Data),
    Monster = util:get_val(monster, Data),
    Heroes = mod_combat:init_monster(Monster),
    Power = mod_hero:calc_power(Heroes),
    MonBin = ?ESC_SINGLE_QUOTES(term_to_binary(Monster)),
    IdL = integer_to_list(Id),
    LevL = integer_to_list(Lev),
    ExpL = integer_to_list(Exp),
    PicL = integer_to_list(Pic),
    PowerL = integer_to_list(Power),
    Name = ?ESC_SINGLE_QUOTES(list_to_binary("^." ++ IdL ++ ".$")),
    Sql = list_to_binary([<<"SELECT count(*) FROM `arena` WHERE robot = 1 and rid = ">>, IdL]),
    case db:get_one(Sql) of
        {ok, 0} ->
            Sql2 = list_to_binary([<<"INSERT arena(`robot` , `rid` , `lev` , `exp` , `power`, `picture`, `name`, `s`) "
                                     "VALUES (1, ">>, IdL, <<",">>, LevL, <<",">>, ExpL, <<",">>, PowerL, <<",">>, PicL, <<",'">>, Name, <<"','">>, MonBin, <<"');">>]),
            db:execute(Sql2);
        {ok, 1} ->
            Sql2 = list_to_binary([
                    <<"update arena set lev = ">>, LevL
                    ,<<", exp = ">>, ExpL
                    ,<<", power = ">>, PowerL
                    ,<<", picture = ">>, PicL
                    ,<<", s = '">>, MonBin, <<"'">>
                    ,<<" where robot = 1 and rid = ">>, IdL
            ]),
            db:execute(Sql2);
        {ok, Reason} ->
            ?WARN("~w", [Reason]),
            ok
    end,
    io:format(".", []),
    import_robot(Ids);
import_robot([]) -> ok.

zip_heroes(Heroes) ->
    zip_heroes(Heroes, []).

zip_heroes([H | Heroes], Rt) ->
    Bin = mod_hero:zip(H),
    Rt1 = [Bin | Rt],
    zip_heroes(Heroes, Rt1);
zip_heroes([], Rt) ->
    term_to_binary(Rt).

unzip_heroes(Bin) ->
    L = binary_to_term(Bin),
    unzip_heroes(L, []).

unzip_heroes([H | T], Rt) ->
    Hero = mod_hero:unzip([0, H]),
    Rt1 = [Hero | Rt],
    unzip_heroes(T, Rt1);
unzip_heroes([], Rt) -> Rt.

fix_arena_pos(Heroes) ->
    Heroes1 = [Hero || Hero <- Heroes, Hero#hero.pos > 0],
    Heroes2 = case Heroes1 =:= [] of
        true -> util:rand_element(4, Heroes);
        false -> Heroes1
    end,
    Poses = util:shuffle([11, 12, 13, 14, 15, 16, 17, 18, 19]),
    fix_arena_pos1(Heroes2, Poses, []).

fix_arena_pos1([H = #hero{pos = Pos} | Heroes], [PosH | PosT], Rt) ->
    Pos1 = if
        Pos >= 11, Pos =< 19 ->
            case lists:member(Pos, [PosH | PosT]) of
                true -> Pos + 10;
                false ->
                    ?DEBUG("Pos:~w -> ~w ***", [Pos, PosH]),
                    PosH + 10
            end;
        true -> PosH + 10
    end,
    PosL = lists:delete(Pos1 - 10, [PosH | PosT]),
    Rt1 = [H#hero{pos = Pos1} | Rt],
    fix_arena_pos1(Heroes, PosL, Rt1);
fix_arena_pos1([], _, Rt) ->
    mod_battle:print_hero(Rt),
    Rt.

update_rank() ->
    ?SERVER ! update_rank.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-record(state, {
        %% 悬赏挑战榜 [{ArenaId, Name1, Name2}]
        offer_report = [
            %% {100000, <<"TEST1">>, <<"TEST2">>},
            %% {100001, <<"TEST3">>, <<"TEST4">>}
        ]
    }).

init([]) ->
    ?INFO("start ~w...", [?MODULE]),
    %% put(rank_data, []), %% 排行榜数据：id, name, exp,
    State = #state{},
    self() ! init_rank,
    {ok, State}.

%% 获得我的排名
handle_call({get_rank_pos, Rid, Lev}, _From, State) ->
    Pos = case get({rank, Lev}) of
        undefined -> 0;
        Data ->
            case lists:keyfind(Rid, 2, Data) of
                false -> 0;
                {X, _, _, _, _} -> X
            end
    end,
    {reply, Pos, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% 增加悬赏挑战记录
handle_cast({add_offer_report, Sender, Id, Name1, Name2}, State) ->
    Report = lists:sublist(State#state.offer_report, 1, 50),
    case lists:keymember(Id, 1, Report) of
        true ->
            sender:pack_send(Sender, 23017, [2]),
            {noreply, State};
        false ->
            OfferReport = [{Id, Name1, Name2} | Report],
            State1 = State#state{offer_report = OfferReport},
            sender:pack_send(Sender, 23017, [0]),
            {noreply, State1}
    end;

%% 删除悬赏挑战记录
handle_cast({del_offer_report, Id}, State) ->
    OfferReport = lists:keydelete(Id, 1, State#state.offer_report),
    State1 = State#state{offer_report = OfferReport},
    {noreply, State1};

handle_cast(Msg, State) ->
    ?WARN("Undefined msg:~w", [Msg]),
    {noreply, State}.

%% 清除悬赏榜
handle_info(offer_clean, State) ->
    State1 = State#state{offer_report = []},
    {noreply, State1};

%% 发送悬赏挑战数据
handle_info({send_offer_report, Id, _Name}, State) ->
    %% 这里不需要遍历两次
	%% Lists1 = lists:filter(fun({_,B,_}) -> B =/= Name end, State#state.offer_report),
	%% Lists2 = lists:filter(fun({_,_,C}) -> C =/= Name end, Lists1),
    %% F = fun
    %%         ({_, B, C}) when B =:= Name; C =:= Name -> false;
    %%         (_) -> true
    %%     end,
	%% L = lists:filter(F, State#state.offer_report),
    sender:pack_send(Id, 23018, [State#state.offer_report]),
    {noreply, State};

%% 发送竞技场排行榜
handle_info({send_rank, Sender, Lev}, State) ->
    Rank = case get({rank, Lev}) of
        undefined ->
            Data1 = get_rank_from_db(Lev),
            put({rank, Lev}, Data1),
            Data1;
        Data -> Data
    end,
    ?DEBUG("send_rank:~w", [length(Rank)]),
    sender:pack_send(Sender, 23016, [Rank]),
    {noreply, State};

handle_info(init_rank, State) ->
    F = fun(Lev) ->
            case get({rank, Lev}) of
                undefined ->
                    Data1 = get_rank_from_db(Lev),
                    put({rank, Lev}, Data1),
                    Data1;
                _ -> ok
            end
    end,
	LevMax = length(data_arena_exp:get(ids)),
    util:for(1, LevMax, F),
    ?DEBUG("init_rank ok!", []),
    {noreply, State};

handle_info(update_rank, State) ->
    F = fun(Lev) ->
            Data1 = get_rank_from_db(Lev),
            put({rank, Lev}, Data1)
    end,
	LevMax = length(data_arena_exp:get(ids)),
    util:for(1, LevMax, F),
    ?DEBUG("update_rank ok!", []),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% 添加排名字段
fix_rank(Data) ->
    fix_rank(Data, 0, []).
fix_rank([R|T], Key, Rs) ->
    Pos = Key + 1,
    [Id, Name, Exp, Power] = R,
    fix_rank(T, Pos, [{Pos, Id, Name, Exp, Power}|Rs]);
fix_rank([], _, Rs) ->
    lists:reverse(Rs).

get_rank_from_db(Lev) ->
    %% Sql = list_to_binary([<<"SELECT `id`, `name`, `exp` FROM `arena` WHERE `robot` = 0 AND `lev` = ">>, Lev, <<"ORDER BY `exp` DESC LIMIT 0, 100;">>]),
    Sql = list_to_binary([<<"SELECT `id`, `name`, `exp`, `power` FROM `arena` WHERE `lev` = ">>,
            integer_to_list(Lev), <<" ORDER BY `exp` DESC LIMIT 0, 100;">>]),
    case db:get_all(Sql) of
        {ok, Data} -> fix_rank(Data);
        {error, Reason} -> 
            ?WARN("~w", [Reason]),
            []
    end.

%% 竞技场对手信息存储
arena_stores(Rs) ->
	#role{
	   id	 = Id
	   ,arena = Arena
	   ,arena_lost_report = Report
	  } = Rs,
	Val1 = ?ESC_SINGLE_QUOTES(zip(Arena)),
	Val2 = ?ESC_SINGLE_QUOTES(zip2(Report)),
	Sql = list_to_binary([<<"UPDATE `arena` SET `rival` = '">>
				, Val1,<<"', `report` = '">>, Val2,
				<<"' WHERE robot = 0 AND rid = ">>,integer_to_list(Id),<<";">>]),
	db:execute(Sql).

%% zip / unzip
%% 解包#role.arena,对手信息
unzip(Bin) ->
    <<Version:8, Bin1/binary>> = Bin,
    case Version of
        1 ->
			unzip1(Bin1, []);
        _ ->
            ?ERR("undefined version: ~w", [Version]),
            undefined
    end.
unzip1(<<X1:32, X2_:16, X2:X2_/binary, X3:8, X4:8, RestBin/binary>>, Rt) ->
	Rt1 = [[X1, X2, X3, X4] | Rt],
	unzip1(RestBin, Rt1);
unzip1(<<>>, Rt) ->
	Rt.

%% 打包成binary
-define(RIVAL_ZIP_VERSION, 1).
zip(L) ->
	F = fun([Id, Name, Pic, Beat]) ->
				Name1 = byte_size(Name),
				<<Id:32, Name1:16, Name:Name1/binary, Pic:8, Beat:8>>
		end,
	Bin = list_to_binary([F([A, B, C, D]) || [A, B,C, D] <- L]),
	<<?RIVAL_ZIP_VERSION:8, Bin/binary>>.

%% zip2 / unzip2
%% 解包#role.arena_lost_report,战报信息
unzip2(Bin) ->
    <<Version:8, Bin1/binary>> = Bin,
    case Version of
        1 ->
			unzip2(Bin1, []);
        _ ->
            ?ERR("undefined version: ~w", [Version]),
            undefined
    end.
unzip2(<<X1:32, X2_:16, X2:X2_/binary, RestBin/binary>>, Rt) ->
	Rt1 = [[X1, X2] | Rt],
	unzip2(RestBin, Rt1);
unzip2(<<>>, Rt) ->
	Rt.

%% 打包成binary
-define(REPORT_ZIP_VERSION, 1).
zip2(L) ->
	F = fun({Id, Name}) ->
				Name1 = byte_size(Name),
				<<Id:32, Name1:16, Name:Name1/binary>>
		end,
	Bin = list_to_binary([F({A, B}) || {A, B}<- L]),
	<<?REPORT_ZIP_VERSION:8, Bin/binary>>.

%% vim: set ft=erlang foldmethod=marker foldmarker=%%',%%.:
