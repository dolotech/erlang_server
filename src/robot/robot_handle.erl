%%----------------------------------------------------
%% Robot handle 
%%
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(robot_handle).
-export([handle/3]).

-include("common.hrl").
-include("robot.hrl").

%%' 10协议_登陆认证

handle(11000, [LoginState, Growth, Rid], R) ->
    %% ?INFO("11001:~w", [[LoginState, Growth, Rid]]),
    case LoginState of
        1 -> 
            %% 正常登陆
            io:format("1"),
            robot ! {login_ok, R#robot.aid, Rid, self()},
            %% 赠送英雄
            R#robot.pid_sender ! {cmd, 14023, [30005]},
            %% 请求基础数据
            R#robot.pid_sender ! {cmd, 11003, []},
            %% 请求玩家所有英雄
            R#robot.pid_sender ! {cmd, 14002, []},
            %% R#robot.pid_sender ! {cmd, 13001, [1]},
            %% R#robot.pid_sender ! {cmd, 13024, []},
            %% R#robot.pid_sender ! {cmd, 11007, []},
            R#robot{id = Rid, pid = self()};
        2 -> 
            %% 正常登陆
            io:format("2"),
            robot ! {login_ok, R#robot.aid, Rid, self()},
            %% 赠送英雄
            R#robot.pid_sender ! {cmd, 14023, [30005]},
            %% 请求基础数据
            R#robot.pid_sender ! {cmd, 11003, []},
            %% 请求玩家所有英雄
            R#robot.pid_sender ! {cmd, 14002, []},
            %% R#robot.pid_sender ! {cmd, 13001, [1]},
            %% R#robot.pid_sender ! {cmd, 13024, []},
            %% R#robot.pid_sender ! {cmd, 11007, []},
            R#robot{id = Rid, pid = self()};
        3 ->
            %% 帐号不存在，进行注册
            %% 创建角色
            %% Sex = util:rand(1, 2),
            %% SexS = case Sex of
            %%     1 -> <<"男">>;
            %%     2 -> <<"女">>;
            %%     _ -> <<"未知">>
            %% end,
            %% Name = list_to_binary([?AID_PREFIX, integer_to_list(R#robot.aid)]),
            %% ?INFO("Reg:~s - ~s", [SexS, Name]),
            %% R#robot.pid_sender ! {cmd, 11002, [Name, Sex]},
            %% R#robot{sex = Sex, name = Name};
            %% 创建角色
            AidB = list_to_binary([?AID_PREFIX, integer_to_list(R#robot.aid)]),
            Key = "23d7f859778d2093",
            Rand = util:rand(0, 255),
            Signature = util:md5(list_to_binary([
                        "1" 
                        ,integer_to_list(Rand)
                        ,Key
                        ,AidB
                    ])),
            R#robot.pid_sender ! {cmd, 11000, [1, Rand, 1, AidB, AidB, Signature]},
            ?DEBUG("[REG] Rid:~w AccId:~s", [Rid, AidB]),
            %% R#robot.pid_sender ! {cmd, 11002, [<<>>, 0]},
            R#robot{sex = 0, name = <<>>};
        _ -> 
            io:format("login failure! ~w~n", [[LoginState, Growth, Rid]]),
            R
    end;
%%.

handle(11001, [LoginState, Growth, Rid], R) ->
    %% ?INFO("11001:~w", [[LoginState, Growth, Rid]]),
    case LoginState of
        0 -> 
            case Growth of
                0 ->
                    %% 创建角色
                    %% Sex = util:rand(1, 2),
                    %% SexS = case Sex of
                    %%     1 -> <<"男">>;
                    %%     2 -> <<"女">>;
                    %%     _ -> <<"未知">>
                    %% end,
                    %% Name = list_to_binary([?AID_PREFIX, integer_to_list(R#robot.aid)]),
                    %% ?INFO("Reg:~s - ~s", [SexS, Name]),
                    %% R#robot.pid_sender ! {cmd, 11002, [Name, Sex]},
                    %% R#robot{sex = Sex, name = Name};
                    %% 创建角色
                    Name = list_to_binary([?AID_PREFIX, integer_to_list(R#robot.aid)]),
                    ?DEBUG("[REG] Rid:~w AccId:~s", [Rid, Name]),
                    R#robot.pid_sender ! {cmd, 11002, [<<>>, 0]},
                    R#robot{sex = 0, name = <<>>};
                _ -> 
                    %% 正常登陆
                    io:format("+"),
                    robot ! {login_ok, R#robot.aid, Rid, self()},
                    %% 赠送英雄
                    R#robot.pid_sender ! {cmd, 14023, [30005]},
                    %% 请求基础数据
                    R#robot.pid_sender ! {cmd, 11003, []},
                    %% 请求玩家所有英雄
                    R#robot.pid_sender ! {cmd, 14002, []},
                    %% R#robot.pid_sender ! {cmd, 13001, [1]},
                    %% R#robot.pid_sender ! {cmd, 13024, []},
                    %% R#robot.pid_sender ! {cmd, 11007, []},
                    R#robot{id = Rid, pid = self()}
            end;
        1 -> 
            io:format("login failure! ~w~n", [[LoginState, Growth, Rid]]),
            R
    end;
%%.

handle(11002, [0], _R) ->
    ?INFO("Create ok!"),
    %% 创建角色成功
    ok;

%% 钻石
%% 金币
%% 关卡ID
%% 疲劳
%% 装备背包格子数
%% 道具背包格子数
%% 材料背包格子数
%% 竞技场名字
%% 竞技场头像id
handle(11003, [Diamond, Gold, Gid, Power, BagEqu, BagProp, BagMat, Name, Pic], R) ->
    %% ?INFO("===*=== Diamond:~w, Gold:~w ===*===", [Diamond, Gold]),
    Aid = list_to_binary([?AID_PREFIX, integer_to_list(R#robot.aid)]),
    case Diamond < 100 of
        true ->
            timer:apply_after(30000, 
                lib_admin, add_diamond, [R#robot.id, 100000]);
        false ->
            ok
    end,
    case Gold < 500 of
        true ->
            timer:apply_after(3000, 
                lib_admin, add_gold, [R#robot.id, 1000000]);
        false ->
            ok
    end,
    case Name of
        <<>> ->
            %% 名字为空，则去竞技场注册
            NewPic = util:rand(1, 8),
            erlang:send_after(util:rand(1000, 3000), 
                R#robot.pid_sender, {cmd, 23001, [Aid, NewPic]}),
            ok;
        _ ->
            ok
    end,
    R#robot{
        gold            = Gold
        ,diamond        = Diamond
		,bag_equ_max    = BagEqu
		,bag_prop_max   = BagProp
		,bag_mat_max    = BagMat
        ,power          = Power
        ,tollgateid     = Gid
        ,name           = Name
        ,picture        = Pic
    };

handle(11002, [1], _R) ->
    ?INFO("Please rename!"),
    ok;

%% 测试物品
handle(13001, [1, _Equs, _Props], _R) ->
    %% R#robot.pid_sender ! {cmd, 13001, [2]},
    %% EquIds = util:rand_element(3, [Id || [Id|_] <- Equs]),
    %% GemIds = util:rand_element(3, get_gemids(Items, [])),
    %% ShopIds = util:rand_element(3, data_shop:get(ids)),
    %% ?INFO("EquIds:~w", [EquIds]),
    %% lists:foreach(fun(Id)-> 
    %%             %% ?INFO("strengthen:~w", [Id]),
    %%             %% Pay = util:rand(0, 1),
    %%             Pay = 1,
    %%             R#robot.pid_sender ! {cmd, 13005, [Id, 0, Pay]}
    %%     end, EquIds),
    %% lists:foreach(fun(GemId)-> 
    %%             lists:foreach(fun(EquId)-> 
    %%                         R#robot.pid_sender ! {cmd, 13010, [EquId, GemId, 0]}
    %%                 end, EquIds)
    %%     end, GemIds),
    %% %% 购买商品
    %% lists:foreach(fun(ShopId)-> 
    %%             R#robot.pid_sender ! {cmd, 13021, [ShopId, 1]}
    %%     end, ShopIds),
    ok;

%% handle(13005, [Code, Id, Lev, Time], _R) ->
%%     ?INFO("[Code:~w, Id:~w, Lev:~w, Time:~w]", [Code, Id, Lev, Time]),
%%     ok;
%% 
%% handle(13010, Data, _R) ->
%%     ?INFO("[13010] ~w", [Data]),
%%     ok;

%% handle(13020, Data, _R) ->
%%     ?INFO("[shop] ~w", [Data]),
%%     ok;

handle(14001, _Data, R) ->
    %% ?INFO("[set_pos] ~w", [Data]),
    ?DEBUG("布阵OK，请求进入主线战斗", []),
    R#robot.pid_sender ! {cmd, 22002, [1, 1, 0]},
    ok;

handle(14002, [Heroes], R) ->
    ?DEBUG("收到所有英雄", []),
    Heroes1 = util:rand_element(4, Heroes),
    %% ?INFO("~w", [Heroes]),
    %% 净化
    lists:foreach(fun([Id | _]) ->
                          %% ?INFO("Purge Hero Id: ~w", [Id]),
                          R#robot.pid_sender ! {cmd, 14010, [Id]}
                  end, Heroes1),
    %% 布阵
    SetPosF = fun(I, {[[Id | _] | HT], Rt}) ->
            {ok, {HT, [[Id, I] | Rt]}}
    end,
    {ok, {_, Poses}} = util:for(11, length(Heroes1)+10, SetPosF, {Heroes1, []}),
    ?DEBUG("布阵:~w", [Poses]),
    R#robot.pid_sender ! {cmd, 14001, [Poses]},
    %% case Heroes of
    %%     [[Hid | _] | _] ->
    %%         R#robot.pid_sender ! {cmd, 14015, [Hid]};
    %%     _ -> ok
    %% end,
    %% ForgeIds = util:rand_element(3, data_forge:get(ids)),
    %% lists:foreach(fun(Id)-> 
    %%             R#robot.pid_sender ! {cmd, 13012, [Id]}
    %%     end, ForgeIds),
    %% TavernHeroIds = util:rand_element(3, data_tavern:get(ids)),
    %% lists:foreach(fun(Id)-> 
    %%             R#robot.pid_sender ! {cmd, 14020, [Id]}
    %%     end, TavernHeroIds),
    %% 请求英雄酒馆
    R#robot.pid_sender ! {cmd, 14020, [0]},
    R#robot.pid_sender ! {cmd, 14020, [1]},
    ok;

handle(14020, _Data, _R) ->
    %% ?INFO("[14020] ~w", [_Data]),
    %% R#robot.pid_sender ! {cmd, 14022, [1]},
    ok;

%% handle(14022, Data, _R) ->
%%     ?INFO("[14022] ~w", [Data]),
%%     ok;

%% handle(14010, [Code, EH, Step, ER], _R) ->
%%     ?INFO("[purge result] Code:~w, EH:~w, Step:~w, ER:~w", [Code, EH, Step, ER]),
%%     ok;
%% 
%% handle(14015, Data, _R) ->
%%     ?INFO("[14015] ~w", [Data]),
%%     ok;

handle(22002, [0, 0, 0, [], []], _R) ->
    ok;

handle(22002, _Data, R) ->
    %% 宝珠抽取
    lists:foreach(fun(Id)-> 
                R#robot.pid_sender ! {cmd, 13022, [Id]}
        end, [1,2,3,4,5]),
    %% erlang:send_after(5000, R#robot.pid_sender, {cmd, 22002, [1, 0, 1]}),
    ok;

%% 竞技场
handle(23001, [Code], R) ->
    case Code of
        0 -> ok;
        1 -> ?INFO("Repeated Arena Nama: ~s", [R#robot.aid]);
        _ -> ?INFO("Code: ~s", [Code])
    end,
    ok;

handle(Cmd, Data, _R) ->
    ShowCmdList = [
        1
        %% ,14020
        %% ,14022
        %% ,13022
        %% ,13024
        %% ,22002 %% 战斗
    ],
    case lists:member(Cmd, ShowCmdList) of
        true -> io:format("Recv [Cmd:~w Data:~w]~n", [Cmd, Data]);
        false -> ok
    end.


%% get_equids([], Rt) -> Rt;
%% get_equids([[Id, Tid | _] | Items], Rt) ->
%%     Sort = itemtid2sort(Tid),
%%     case lists:member(Sort, [1,2,3,4,5,6,7,8,9,10,11,12]) of
%%         true -> get_equids(Items, [Id | Rt]);
%%         false -> get_equids(Items, Rt)
%%     end.
%% 
%% get_gemids([], Rt) -> Rt;
%% get_gemids([[Id, _Pos, Tid, _, _Atk, _hp, _Agi, _Lev | _] | Items], Rt) ->
%%     Sort = itemtid2sort(Tid),
%%     case lists:member(Sort, [41, 42]) of
%%         true -> get_gemids(Items, [Id | Rt]);
%%         false -> get_gemids(Items, Rt)
%%     end.
%% 
%% itemtid2sort(Tid) ->
%%     Item = data_item:get(Tid),
%%     util:get_val(sort, Item, 0).

%%% vim: set filetype=erlang foldmarker=%%',%%. foldmethod=marker:
