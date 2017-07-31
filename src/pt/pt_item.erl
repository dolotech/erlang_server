%%----------------------------------------------------
%% 协议13 - 物品相关
%%
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-module(pt_item).
-export([handle/3]).

-include("common.hrl").
-include("equ.hrl").
-include("prop.hrl").

handle(13001, [1], Rs) ->
    F = fun(I, {Es, Ps}) ->
            case I#item.tid > 99999 of
                true -> {[mod_item:pack_equ(I)|Es], Ps};
                false -> {Es, [mod_item:pack_prop(I)|Ps]}
            end
    end,
    {Data1, Data2} = lists:foldl(F, {[],[]}, Rs#role.items),
    % ?DEBUG("items:~w ", [Rs#role.items]),
    % ?DEBUG("Data1:~w, Data2:~w ", [Data1, Data2]),
	% ?DEBUG("bagequ:~w,bagprop:~w,bagmat:~w",[Rs#role.bag_equ_max,Rs#role.bag_prop_max,Rs#role.bag_mat_max]),
	% Equ = mod_item:count_unit(?TAB_EQU, Rs#role.items),
	% Prop = mod_item:count_unit(?TAB_PROP, Rs#role.items),
	% Mat = mod_item:count_unit(?TAB_MAT, Rs#role.items),
	% ?DEBUG("equ:~w,prop:~w,mat:~w",[Equ,Prop,Mat]),
    {ok, [1, Data1, Data2]};
handle(13001, [2], Rs) ->
    case produce_item(Rs) of
        {ok, Rs1, Prop, Equ} ->
            Data1 = [mod_item:pack_equ(X) || X <- Equ],
            Data2 = [mod_item:pack_prop(X) || X <- Prop],
            {ok, [2, Data1, Data2], Rs1};
		%% TODO:背包已满
        {error, full} ->
            ?WARN("bag full", []),
            {ok, [2, [], []]};
        {error, Reason} ->
            ?WARN("produce error: ~w", [Reason]),
            {ok, [2, [], []]}
    end;


%% 装备物品
%%
%% 装备(当前位置无装备)：
%% equipID: 待装备的物品唯一ID
%% heroID: HeroId
%% equipReplaceID: 0
%%
%% 装备(当前位置有装备)：
%% equipID: 待装备的物品唯一ID
%% heroID: HeroId
%% equipReplacedID: 原来的装备ID
%%
%% 卸载装备:
%% equipID: 装备的唯一ID
%% heroID: 0
%% equipOriginalID: 0
handle(13002, [EquId, HeroId, OldEquId], Rs) ->
    % ?DEBUG("EquID:~w,HeroId:~w,OldEquId:~w",[EquId, HeroId, OldEquId]),
    Items = Rs#role.items,
    % ?DEBUG("===Item:~w,", [mod_item:get_item(EquId, Items)]),
    case mod_item:get_item(EquId, Items) of
        false ->
            ?WARN("No Item:~w", [EquId]),
            {ok, [127]};
        Item ->
            Items1 = case mod_item:get_item(OldEquId, Items) of
                false -> Items;
                OldItem ->
                    Equ = OldItem#item.attr#equ{hero_id = 0},
                    OldItem1 = OldItem#item{attr = Equ, changed = 1},
                    mod_item:set_item(OldItem1, Items)
            end,
            %% ?DEBUG("13002: EquId(~w) -> Hid(~w)", [EquId, HeroId]),
            Equ1 = Item#item.attr#equ{hero_id = HeroId},
            Item1 = Item#item{attr = Equ1, changed = 1},
            Items2 = mod_item:set_item(Item1, Items1),
            % ?DEBUG("===Items2:~w,", [Items2]),
            {ok, [0], Rs#role{items = Items2}}
    end;

%% 装备强化
%% -> [Code, EquId, Level, RestTime, Pay]
%% Code:
%% 1 = 强化失败（注：不是错误）
%% 2 = 金币不足
%% 3 = 疲劳CD
%% 4 = 材料不足
%% 5 = 已到最高等级
%% >=127 = 异常
%% handle(13005, [EquId, Luck, Pay], Rs) ->
%%     Items = Rs#role.items,
%%     case mod_item:get_item(EquId, Items) of
%%         false -> {ok, [127, 0, 0, 0]};
%%         EquItem ->
%%             #item{sort = Sort, attr = Attr} = EquItem,
%%             #equ{lev = Lev, atime = ATime} = Attr,
%%             AddRate = case mod_item:check_sort(21, Luck, Items) of
%%                 true -> 5;
%%                 false -> 0
%%             end,
%%             RestTime = mod_item:get_strengthen_rest_time(ATime),
%%             if
%%                 RestTime > 0, Pay == 0 ->
%%                     {ok, [3, 0, 0, 0]};
%%                 true ->
%%                     case data_strengthen:get({Sort, Lev+1}) of
%%                         [{Gold, ItemDel, Num, AddTime, RateF, Rate, Rise}] ->
%%                             Diamond = case RestTime > 0 of
%%                                 true -> util:ceil(RestTime / 10);
%%                                 false -> 0
%%                             end,
%%                             case lib_role:spend(gold, Gold, Rs) of
%%                                 {ok, Rs1} ->
%%                                     case lib_role:spend(diamond, Diamond, Rs1) of
%%                                         {ok, Rs2} ->
%%                                             case mod_item:del_items(by_tid, [{ItemDel, Num}], Items) of
%%                                                 {ok, Items1, Notice} ->
%%                                                     case mod_item:strengthen(EquItem, Rate+AddRate, RateF, Rise, AddTime) of
%%                                                         {ok, Status, EquItem1} ->
%%                                                             Items2 = mod_item:set_item(EquItem1, Items1),
%%                                                             Rs3 = Rs2#role{items = Items2, save = [role, items]},
%%                                                             mod_item:send_notice(Rs#role.pid_sender, Notice),
%%                                                             lib_role:notice(Rs3),
%% 															%% 强化成就推送
%% 															Rs4 = case Status of
%% 																	  0 -> mod_attain:attain_state(16, 1, Rs3);
%% 																	  1 -> mod_attain:attain_state(14, 1, Rs3);
%% 																	  _ -> Rs3
%% 																  end,
%% 															Rs5 = case Diamond > 0 of
%% 																	  true ->
%% 																		  Rs7 = mod_attain:attain_state(29, 1, Rs4),
%% 																		  mod_attain:attain_state(52, 1, Rs7);
%% 																	  false -> Rs4
%% 																  end,
%% 															Rs6 = case Luck > 0 of
%% 																	  true -> mod_attain:attain_state(28, 1, Rs5);
%% 																	  false -> Rs5
%% 																  end,
%% 															Rs9 = mod_attain:attain_state(42, 1, Rs6),
%%                                                             Rs8 = lib_role:spend_ok(gold, 1, Rs, Rs9),
%%                                                             Rs0 = lib_role:spend_ok(diamond, 1, Rs, Rs8),
%% 															{ok, [Status, EquId,
%%                                                                   EquItem1#item.attr#equ.lev, AddTime], Rs0};
%%                                                         {error, Reason} ->
%%                                                             ?ERR("~w", [Reason]),
%%                                                             {ok, [127, 0, 0, 0]}
%%                                                     end;
%%                                                 {error, _} ->
%%                                                     {ok, [4, 0, 0, 0]}
%%                                             end;
%%                                         {error, _} ->
%%                                             {ok, [6, 0, 0, 0]}
%%                                     end;
%%                                 {error, _} ->
%%                                     {ok, [2, 0, 0, 0]}
%%                             end;
%%                         undefined ->
%%                             {ok, [5, 0, 0, 0]};
%%                         _Data ->
%%                             ?WARN("undefined strengthen data: ~w", [_Data]),
%%                             {ok, [5, 0, 0, 0]}
%%                     end
%%             end
%%     end;

%% 装备强化
%% -> [Code, EquId, Level, RestTime, Pay]
%% Code:
%% 1 = 强化失败（注：不是错误）
%% 2 = 金币不足
%% 3 = 疲劳CD
%% 4 = 材料不足
%% 5 = 已到最高等级
%% >=127 = 异常
handle(13005, [EquId, EnId1, EnId2, EnId3, Pay], Rs) ->
	Items = Rs#role.items,
	case lists:sum([EnId1,EnId2, EnId3]) > 0 of
		true ->
			case mod_item:get_item(EquId, Items) of
				false -> {ok, [127, 0, 0, 0]};
				EquItem ->
					#item{sort = Sort, attr = Attr} = EquItem,
					#equ{lev = Lev, atime = ATime} = Attr,
					RestTime = mod_item:get_strengthen_rest_time(ATime),
					if
						RestTime > 0, Pay == 0 ->
							{ok, [3, 0, 0, 0]};
						true ->
							case data_strengthen:get({Sort, Lev+1}) of
								[{Gold, AddTime, RateF, Rise}] ->
									F = fun(Id) ->
												case Id > 0 of
													true ->
														case data_enhance_rate:get({Sort, Id}) of
															undefined -> 0;
															N -> N
														end;
													false -> 0
												end
										end,
									RateSum = lists:sum([F(Id) || Id <- [EnId1, EnId2, EnId3]]),
									EquData = data_equ:get(EquId),
									RateRise = util:get_val(enhance_rate_arg, EquData),%% 强化成功系数
									GoldRise = util:get_val(enhance_gold_arg, EquData),%% 强化金币系数
									GoldVal = util:ceil(Gold * GoldRise),
									Rate = util:ceil(RateSum * RateRise),
									Diamond = case RestTime > 0 of
												  true -> util:ceil(RestTime / 10);
												  false -> 0
											  end,
									case lib_role:spend(gold, GoldVal, Rs) of
										{ok, Rs1} ->
											case lib_role:spend(diamond, Diamond, Rs1) of
												{ok, Rs2} ->
													case mod_item:del_items(by_tid, [{EnId1, 1},{EnId2, 1},{EnId3, 1}], Items) of
														{ok, Items1, Notice} ->
															case mod_item:strengthen(EquItem, Rate, RateF, Rise, AddTime) of
																{ok, Status, EquItem1} ->
																	Items2 = mod_item:set_item(EquItem1, Items1),
																	Rs3 = Rs2#role{items = Items2, save = [role, items]},
																	mod_item:send_notice(Rs#role.pid_sender, Notice),
																	lib_role:notice(Rs3),
																	%% 强化成就推送
																	Rs4 = case Status of
																			  0 -> mod_attain:attain_state(16, 1, Rs3);
																			  1 -> mod_attain:attain_state(14, 1, Rs3);
																			  _ -> Rs3
																		  end,
																	Rs6 = case Diamond > 0 of
																			  true ->
																				  Rs5 = mod_attain:attain_state(29, 1, Rs4),
																				  mod_attain:attain_state(52, 1, Rs5);
																			  false -> Rs4
																		  end,
																	Rs7 = mod_attain:attain_state(42, 1, Rs6),
																	Rs8 = lib_role:spend_ok(gold, 1, Rs, Rs7),
																	Rs0 = lib_role:spend_ok(diamond, 1, Rs, Rs8),
																	{ok, [Status, EquId,
																		  EquItem1#item.attr#equ.lev, AddTime], Rs0};
																{error, Reason} ->
																	?ERR("~w", [Reason]),
																	{ok, [127, 0, 0, 0]}
															end;
														{error, _} ->
															{ok, [4, 0, 0, 0]}
													end;
												{error, _} ->
													{ok, [6, 0, 0, 0]}
											end;
										{error, _} ->
											{ok, [2, 0, 0, 0]}
									end;
								undefined ->
									{ok, [5, 0, 0, 0]};
								_Data ->
									?WARN("undefined strengthen data: ~w", [_Data]),
									{ok, [5, 0, 0, 0]}
							end
					end
			end;
		false ->
			{ok, [128, 0, 0, 0]}
	end;

handle(13006, [EquId], Rs) ->
    Items = Rs#role.items,
    case mod_item:get_item(EquId, Items) of
        false -> {ok, [127, 0]};
        EquItem ->
            #item{attr = Attr} = EquItem,
            #equ{atime = ATime} = Attr,
            RestTime = mod_item:get_strengthen_rest_time(ATime),
            RestTime1 = case RestTime < 0 of
                            true -> 0;
                            false -> RestTime
                        end,
            ?DEBUG("RestTime:~w", [RestTime]),
            {ok, [0, RestTime1]}
    end;

%% 当SORT为4时
%% 1,攻击
%% 2,血量
%% 3,防御
%% 4,穿刺
%% 5,命中
%% 6,闪避
%% 7,暴击
%% 8,暴强
%% 9,免爆
%% 10,韧性

%% 镶嵌
%% -> [Code, EquId]
%% Code:
%% 0 = 成功
%% 1 = 镶嵌失败（注：不是错误）
%% 2 = 金币不足
%% 3 = 钻石不足
%% 4 = 装备的孔不足
%% >=127 = 程序异常
handle(13010, [EquId, GemId], Rs) ->
    Items = Rs#role.items,
    EquItem = mod_item:get_item(EquId, Items),
    GemItem = mod_item:get_item(GemId, Items),
    % ?DEBUG("==GemItem:~w, tid:~w ", [GemItem, GemItem#item.tid]),
    if
        EquItem == false ->
            {ok, [127, 0]};
        GemItem == false ->
            {ok, [128, 0]};
        EquItem#item.tid < ?MIN_EQU_ID ->
            {ok, [129, 0]};
        GemItem#item.tid > ?MIN_EQU_ID ->
            {ok, [130, 0]};
        GemItem#item.sort =/= 4 ->
            {ok, [131, 0]};
        EquItem#item.attr#equ.sockets =< 0 ->
            {ok, [132, 0]};
        true ->
            #item{attr = GemAttr} = GemItem,
            JewelData = data_jewel:get(GemAttr#prop.quality),
            Currency = util:get_val(money1, JewelData),
            Num = util:get_val(num1, JewelData),
            {CType, RtCode} = case Currency of
                1 -> {gold, 2};
                2 -> {diamond, 3}
            end,
            case lib_role:spend(CType, Num, Rs) of
                {ok, Rs1} ->
                    case mod_item:del_items(by_id, [{GemId, 1}], Items) of
                        {ok, Items1, Notice} ->
                            case mod_item:embed(EquItem, GemItem) of
                                {ok, EquItem1} ->
                                    Items2 = mod_item:set_item(EquItem1, Items1),
                                    Rs2 = Rs1#role{items = Items2, save = [role, items]},
                                    mod_item:send_notice(Rs#role.pid_sender, Notice),
                                    mod_item:send_notice(Rs#role.pid_sender, [], [EquItem1]),
                                    lib_role:notice(Rs2),
                                    %% 成就推送
                                    Rs3 = mod_attain:attain_state(47, 1, Rs2),
                                    Rs0 = lib_role:spend_ok(CType, 2, Rs, Rs3),
                                    {ok, [0, EquId], Rs0};
                                {error, no_sock} ->
                                    {ok, [4, EquId]};
                                {error, quality} ->
                                    {ok, [135, EquId]};
                                {error, Reason} ->
                                    ?ERR("~w", [Reason]),
                                    {ok, [127, 0]}
                            end;
                        {error, _} ->
                            {ok, [RtCode, 0]}
                    end;
                {error, _} ->
                    {ok, [2, 0]}
            end
    end;

%% 合成
%% -> [Code]
%% Code:
%% 0 = 成功
%% 1 = 失败（注：不是错误）
%% 2 = 材料不足
%% >=127 = 程序异常
handle(13012, [Tid], Rs) ->
    Items = Rs#role.items,
    Data = data_forge:get(Tid),
    % ?DEBUG("===Data:~w ,Itmes:~w ", [Data,hd(Items)]),
    if
        Data == undefined ->
            {ok, [127, 0]};
        true ->
            {Rate, Dels} = Data,
            case mod_item:del_items(by_tid, Dels, Items) of
                {ok, Items1, Notice} ->
                    Rs1 = Rs#role{items = Items1},
                    case util:rate(Rate) of
                        true ->
                            case mod_item:add_item(Rs1, Tid, 1) of
                                {ok, Rs2, PA2, EA2} ->
                                    mod_item:send_notice(Rs#role.pid_sender, Notice),
                                    mod_item:send_notice(Rs#role.pid_sender, PA2, EA2),
									%% 合成成就推送
									Rs3 = mod_attain:attain_state(23,1, Rs2),
									Rs4 = case Dels of
											  {[11093,_]} -> mod_attain:attain_state(18,1,Rs3);
											  {[11381,_]} -> mod_attain:attain_state(19,1,Rs3);
											  {[11189,_]} -> mod_attain:attain_state(20,1,Rs3);
											  {[11285,_]} -> mod_attain:attain_state(21,1,Rs3);
											  _ -> Rs3
										  end,
									Rs0 = mod_attain:attain_state(43, 1, Rs4),
                                    {ok, [0], Rs0#role{save = [items]}};
								%% TODO:邮件系统
								{error, full} ->
									{ok, [3]};
                                {error, _} ->
                                    {ok, [128]}
                            end;
                        false ->
                            mod_item:send_notice(Rs#role.pid_sender, Notice),
							%% 成就推送
							Rs2 = mod_attain:attain_state(22, 1, Rs1),
							Rs0 = mod_attain:attain_state(43, 1, Rs2),
                            {ok, [1], Rs0}
                    end;
                {error, _} ->
                    {ok, [2]}
            end
    end;

%% 在商城购买商品
%% 消息代码：
%% Code:
%% 0 = 成功
%% 1 = 金币不足
%% 2 = 钻石不足
%% >=127 = 程序异常
%% get(1) -> [{tid, 23010}, {sort, 1}, {num, 5}, {price, 40}];
%% handle(13021, [ShopId, Type], Rs) ->
%%     Data = data_shop:get(ShopId),
%%     if
%%         Data == undefined ->
%%             {ok, [127, []]};
%%         true ->
%%             {Price, Num} = case Type of
%%                                1 ->
%%                                    {util:get_val(price1, Data),
%%                                     util:get_val(num1, Data)};
%%                                2 ->
%%                                    {util:get_val(price2, Data),
%%                                     util:get_val(num2, Data)}
%%                            end,
%%             Diamond = util:ceil(Price * Num),
%%             % ?DEBUG("diamond:~w, mydiamond:~w", [Diamond, Rs#role.diamond]),
%%             case lib_role:spend(diamond, Diamond, Rs) of
%%                 {ok, Rs1} ->
%%                     Tid = util:get_val(tid, Data),
%%                     case util:get_val(sort, Data) of
%%                         1 ->
%%                             %% 宝箱
%%                             ItemData = data_prop:get(Tid),
%%                             Control1 = util:get_val(control1, ItemData),
%%
%%                             Range = gen_range(Control1),
%%                             Tids = gen_item_tid(Num, Range),
%%                             case mod_item:add_items(Rs1, Tids) of
%%                                 {ok, Rs2, PA, EA} ->
%%                                     %% NewTid = [X || #item{tid = X} <- PA ++ EA],
%%                                     mod_item:send_notice(Rs#role.pid_sender, PA, EA),
%%                                     lib_role:notice(diamond, Rs2),
%%                                     {ok, [0, Tids], Rs2#role{save = [role, items]}};
%%                                 {error, _} ->
%%                                     {ok, [128, []]}
%%                             end;
%%                         2 ->
%%                             %% 普通物品
%%                             % ?DEBUG("==add_item:~w",[mod_item:add_item(Rs1, Tid, Num)]),
%%                             case mod_item:add_item(Rs1, Tid, Num) of
%%                                 {ok, Rs2, PA, EA} ->
%%                                     mod_item:send_notice(Rs#role.pid_sender, PA, EA),
%%                                     lib_role:notice(diamond, Rs2),
%%                                     {ok, [0, [[Tid, Num]]], Rs2#role{save = [role, items]}};
%%                                 {error, _} ->
%%                                     {ok, [128, []]}
%%                             end
%%                     end;
%%                 {error, _} ->
%%                     {ok, [2, []]}
%%             end
%%     end;

handle(13021, [ShopId], Rs) ->
	Data = data_shop:get(ShopId),
	if
		Data == undefined ->
			{ok, [127]};
		true ->
			Price = util:get_val(price1, Data),
			Num = util:get_val(num1, Data),
			Diamond = util:ceil(Price * Num),
			case lib_role:spend(diamond, Diamond, Rs) of
				{ok, Rs1} ->
					case util:get_val(tid, Data) of
						1 ->
							%% 购买金币
							Rs2 = lib_role:add_attr(gold, Num, Rs1),
                            Rs3 = lib_role:add_attr_ok(gold, 28, Rs1, Rs2),
                            Rs0 = lib_role:spend_ok(diamond, 28, Rs, Rs3),
							lib_role:notice(Rs0),
							{ok, [0], Rs0};
						3 ->
							%% 幸运星购买
							Rs2 = lib_role:add_attr(luck, Num, Rs1),
							lib_role:notice(luck, Rs2),
							lib_role:notice(diamond, Rs2),
							%% 购买幸运星日常成就推送
							Rs3 = mod_attain:attain_state(57, 1, Rs2),
                            Rs0 = lib_role:spend_ok(diamond, 28, Rs, Rs3),
							{ok, [0], Rs0};
						Tid ->
							%% 普通物品
							% ?DEBUG("==add_item:~w",[mod_item:add_item(Rs1, Tid, Num)]),
							case mod_item:add_item(Rs1, Tid, Num) of
								{ok, Rs2, PA, EA} ->
									mod_item:send_notice(Rs2#role.pid_sender, PA, EA),
									lib_role:notice(diamond, Rs2),
                                    Rs0 = lib_role:spend_ok(diamond, 28, Rs, Rs2),
									{ok, [0], Rs0#role{save = [role, items]}};
								%% TODO:邮件系统
								{error, full} ->
									{ok, [3]};
								{error, _} ->
									{ok, [128]}
							end
					end;
				{error, _} ->
					{ok, [2]}
			end
	end;


%% ------------------------------------------------------------------
%% 魔法宝珠获取
%%
%% jewel: 宝珠抽取状态
%% 如为[]时会初始化为: [{1, 1}, {2, 0}, {3, 0}, {4, 0}, {5, 0}]
%% 格式: [{Quality, Status}, ...]
%%       Quality : 宝珠品质等级
%%       Status : 是否开始(1=是,0=否)
%% ------------------------------------------------------------------
handle(13022, [Quality], Rs) ->
    case Rs#role.jewel of
        [] ->
            %% 末初始化宝珠抽取状态
            {ok, [127, 0, 0, 0]};
        Jewel ->
            case lists:member({Quality, 1}, Jewel) of
                true ->
                    jewel(Quality, Rs);
                false ->
                    %% 请求了末开启的宝珠品质等级
                    {ok, [128, 0, 0, 0]}
            end
    end;

%% 魔法宝珠状态
handle(13024, [], Rs) ->
    case Rs#role.jewel of
        [] ->
            Jewel = [{1, 1}, {2, 0}, {3, 0}, {4, 0}, {5, 0}],
            {ok, [Jewel], Rs#role{jewel = Jewel}};
        D ->
            {ok, [D]}
    end;

%% 连续签到
%%
handle(13025, [], Rs) ->
    NewTime = util:unixtime(),
    AmSix = util:unixtime(noon) - 21600,       %% 今天6点时间戳
    % NewTime = timetest(),
    % ?DEBUG("localtime::~p",[erlang:localtime()]),
    % AmSix = timetest() -21600,
    SignDays = Rs#role.sign_days,
    SignTime = Rs#role.sign_time,
    SignOld  = Rs#role.sign_old_days,
    SignDays1 = if
        SignDays == 0        -> 1;
        SignDays > 7         -> 1;
        SignDays =< SignOld -> SignOld+1;
        true                 -> SignDays
    end,
    SignItem = data_sign:get(SignDays1),
    % ?DEBUG("==SignItem:~p~n", [SignItem]),

    G = if     % 已经签到天数,sign_old_days,发奖成功的时候更新,这个值超过7天重置为0
        SignDays =< SignOld -> SignOld;
        SignDays > SignOld  -> SignDays
    end,

    T = case SignTime > 0 of   % 上次签到时间差
        true -> AmSix - SignTime;
        false -> 0
    end,
    {R,Rs1} = if % 签到成功返回天数
            NewTime >= AmSix , SignTime =< AmSix ->
                case T =< 86400 of
                    true ->
                        D = Rs#role.sign_days + 1,
                        Rss = Rs#role{sign_days = D},
                        {D, Rss};
                    false ->
                        Rss = Rs#role{sign_days = 1},
                        {1, Rss}
                end;
            true -> {0, Rs}    % 签到失败没到签到时间
        end,
    % ?DEBUG("G:~p,sign_old_days:~p",[G,R]),
    % G >= R 签到成功不发奖
    % R =:= 0 签到失败
    case R=/=0 of
        true ->
            case G < R andalso R < 7 of   % 重置R,或者R<7
                true ->
                    Gold = util:get_val(coin, SignItem),
                    Diamond = util:get_val(diamond, SignItem),
                    TidNum = util:get_val(tid_num, SignItem),

                    Rs2 = lib_role:add_attr(gold, Gold, Rs1),
					Rs3 = lib_role:add_attr_ok(gold, 11, Rs, Rs2),
                    Rs6 = lib_role:add_attr(diamond, Diamond, Rs3),
					Rs0 = lib_role:add_attr_ok(diamond, 11, Rs, Rs6),
                    case mod_item:add_items(Rs0, TidNum) of
                        {ok, Rs4, PA, EA} ->
                            case PA =:= [] andalso EA =:= [] of
                                true ->
                                    {ok, [0], Rs4};
                                false ->
                                    mod_item:send_notice(Rs4#role.pid_sender, PA, EA),
                                    Rs5 = Rs4#role{sign_time = NewTime, sign_old_days = R},
                                    lib_role:notice(Rs5),
                                    {ok, [0], Rs5}
                            end;
						%% TODO:邮件系统
						{error, full} ->
							{ok, [3]};
                        {error, _} ->
                            {ok, [127]}
                    end;
                false ->
                    Rs2 = Rs1#role{sign_time = NewTime},
                    {ok, [0], Rs2}
            end;
        false ->
            {ok, [1], Rs1}
    end;

%% 签到界面显示
handle(13026, [], Rs) ->
    SignDays = Rs#role.sign_days,
    SignTime = Rs#role.sign_time,
    ?DEBUG("===SignDays:~p", [SignDays]),
    AmSix = util:unixtime(noon) - 21600,       %% 今天6点时间戳
    if
        SignDays =:= 0 ->
            {ok, [1, 1]};
        true ->
            case SignTime =< AmSix of
                true ->
                    {ok, [1, Rs#role.sign_days+1]};
                false ->
                    {ok, [0, Rs#role.sign_days]}
            end
    end;

%% 背包格子数开放
%% # 消息代码：
%% # Code:
%% # 0 = 成功
%% # 1 = 失败
%% # >=127 = 程序异常
handle(13027, [Type, Line, Tab], Rs) ->
	?DEBUG("Type:~p, Tab:~p, Line:~p ~n", [Type, Tab, Line]),
	if
		Type =:= 1 ->
			case data_bags:get(Line) of
				[{price, Price}] ->
					case lib_role:spend(gold, Price, Rs) of
						{ok, Rs1} ->
							if
								Tab =:= 1 ->
									Newbags = Rs1#role.bag_mat_max + Line * 8,
									%% ?DEBUG("Newbags:~p~n",[Newbags]),
									lib_role:notice(Rs1),
									Rs2 = Rs1#role{bag_mat_max = Newbags},
									Rs3 = mod_attain:attain_state(27,1,Rs2),
                                    Rs0 = lib_role:spend_ok(gold, 6, Rs, Rs3),
									{ok, [0, Tab, Newbags], Rs0};
								Tab =:= 2 ->
									Newbags = Rs1#role.bag_prop_max + Line * 8,
									lib_role:notice(Rs1),
									Rs2 = Rs1#role{bag_prop_max = Newbags},
									Rs3 = mod_attain:attain_state(27,1,Rs2),
                                    Rs0 = lib_role:spend_ok(gold, 6, Rs, Rs3),
									{ok, [0, Tab, Newbags], Rs0};
								Tab =:= 5 ->
									Newbags = Rs1#role.bag_equ_max + Line * 8,
									lib_role:notice(Rs1),
									Rs2 = Rs1#role{bag_equ_max = Newbags},
									Rs3 = mod_attain:attain_state(27,1,Rs2),
                                    Rs0 = lib_role:spend_ok(gold, 6, Rs, Rs3),
									{ok, [0, Tab, Newbags], Rs0};
								true ->
									{ok, [127, 0, 0]}
							end;
						{error, _} ->
							{ok, [1, 0, 0]}
					end;
				undefined ->
					{ok, [128, 0, 0]}
			end;
		true ->
			{ok, [129, 0, 0]}
	end;

%% 批量出售物品
%% # 消息代码:
%% # Code:
%% # 0 = 出售成功
%% # >=127 = 程序异常
handle(13028, [Tab, List], Rs) ->
    ?DEBUG("List:~p~n", [List]),
    %% List :: [[ID1], [ID2], ...]
    Items = Rs#role.items,
    %% 先把收到的参数List转成自己所需的数据
    %% DelsData :: [Id, Num, Sell, Price]
    F1 = fun(Id) ->
            case mod_item:get_item(Id, Items) of
                false ->
                    ?ERR("Error Id: ~w, Tab: ~w, Data: ~w",
                        [Id, Tab, List]),
                    {0, 0, 0, 0};
                I ->
                    if
                        Tab == ?TAB_EQU ->
                            Data = data_equ:get(I#item.tid),
                            Sell = util:get_val(sell, Data),
                            Price = util:get_val(price, Data),
                            {Id, 1, Sell, Price};
                        Tab == ?TAB_MAT orelse Tab == ?TAB_PROP ->
                            Data = data_prop:get(I#item.tid),
                            Sell = util:get_val(sell, Data),
                            Price = util:get_val(price, Data),
                            {Id, I#item.attr#prop.num, Sell, Price};
                        true ->
                            ?ERR("Error Tab: ~w, Data: ~w", [Tab, List]),
                            {0, 0, 0, 0}
                    end
            end
    end,
    DelsData = [F1(Id) || [Id] <- List],
    %% 通过DelsData生成删除特品的参数[{Id, Num}, ...]
    DelIds = [{Id, Num} || {Id, Num, _, _} <- DelsData, Id > 0],
    case mod_item:del_items(by_id, DelIds, Items) of
        {ok, Items1, Notice} ->
            Rs1 = Rs#role{items=Items1},
            %% 计算批量出售所得到的金币或钻石的总额
            F2 = fun
                ({0, _, _, _}, Rt) -> Rt;
                ({_, _, 1, Gold}, {G, D}) -> {Gold + G, D};
                ({_, _, 2, Diamond}, {G, D}) -> {G, Diamond + D};
                (Else, Rt) ->
                    ?WARN("Error Data: ~w, Rt: ~w", [Else, Rt]),
                    Rt
            end,
            {AddGold, AddDiamond} = lists:foldl(F2, {0, 0}, DelsData),
            Rs2 = lib_role:add_attr(gold, AddGold, Rs1),
            Rs3 = lib_role:add_attr(diamond, AddDiamond, Rs2),
			Rs4 = lib_role:add_attr_ok(gold, 29, Rs, Rs3),
			Rs0 = lib_role:add_attr_ok(diamond, 29, Rs, Rs4),
            %% 物品删除通知
            mod_item:send_notice(Rs1#role.pid_sender, Notice),
            %% 通知钱币更新
            lib_role:notice(Rs0),
            %% 一切都OK后，从数据库中删除物品
            mod_item:del_items_from_db(Rs#role.id, Notice),
            {ok, [0], Rs0};
        {error, _} ->
            {ok, [127]}
    end;

%% 批量出售宝珠
%% # 消息代码:
%% # Code:
%% # 0 = 出售成功
%% # >=127 = 程序异常
handle(13032, [Tab, List], Rs) ->
    ?DEBUG("List:~p~n", [List]),
    %% List :: [[ID1], [ID2], ...]
    Items = Rs#role.items,
    %% 先把收到的参数List转成自己所需的数据
    %% DelsData :: [Id, Num, Sell, Price]
    F1 = fun(Id, N) ->
            case mod_item:get_item(Id, Items) of
                false ->
                    ?ERR("Error Id: ~w, Tab: ~w, Data: ~w",
                        [Id, Tab, List]),
                    {0, 0, 0, 0};
                I ->
                    if
                        Tab == ?TAB_EQU ->
                            Data = data_equ:get(I#item.tid),
                            Sell = util:get_val(sell, Data),
                            Price = util:get_val(price, Data),
                            {Id, 1, Sell, Price};
                        Tab == ?TAB_MAT orelse Tab == ?TAB_PROP ->
                            Data = data_prop:get(I#item.tid),
                            Sell = util:get_val(sell, Data),
                            Price = util:get_val(price, Data),
                            N1 = case N =< I#item.attr#prop.num of
                                true -> N;
                                false -> I#item.attr#prop.num
                            end,
                            {Id, N1, Sell, Price};
                        true ->
                            ?ERR("Error Tab: ~w, Data: ~w", [Tab, List]),
                            {0, 0, 0, 0}
                    end
            end
    end,
    DelsData = [F1(Id, N) || [Id, N] <- List],
    %% 通过DelsData生成删除特品的参数[{Id, Num}, ...]
    DelIds = [{Id, Num} || {Id, Num, _, _} <- DelsData, Id > 0],
    case mod_item:del_items(by_id, DelIds, Items) of
        {ok, Items1, Notice} ->
            Rs1 = Rs#role{items=Items1},
            %% 计算批量出售所得到的金币或钻石的总额
            F2 = fun
                ({0, _, _, _}, Rt) -> Rt;
                ({_, _, 1, Gold}, {G, D}) -> {Gold + G, D};
                ({_, _, 2, Diamond}, {G, D}) -> {G, Diamond + D};
                (Else, Rt) ->
                    ?WARN("Error Data: ~w, Rt: ~w", [Else, Rt]),
                    Rt
            end,
            {AddGold, AddDiamond} = lists:foldl(F2, {0, 0}, DelsData),
            Rs2 = lib_role:add_attr(gold, AddGold, Rs1),
            Rs3 = lib_role:add_attr(diamond, AddDiamond, Rs2),
			Rs4 = lib_role:add_attr_ok(gold, 29, Rs, Rs3),
			Rs0 = lib_role:add_attr_ok(diamond, 29, Rs, Rs4),
            %% 物品删除通知
            mod_item:send_notice(Rs1#role.pid_sender, Notice),
            %% 通知钱币更新
            lib_role:notice(Rs0),
            %% 一切都OK后，从数据库中删除物品
            mod_item:del_items_from_db(Rs#role.id, Notice),
            {ok, [0], Rs0};
        {error, _} ->
            {ok, [127]}
    end;

%% Rs1 = lib_role:add_attr(diamond, LuckDia, Rs),
%% 					lib_role:notice(Rs1),
%% 					lib_role:notice(luck, Rs1),
%% 					{ok, [], Rs0};

%%----------------------------------------------------
%% 幸运星抽取
%% # 消息代码：
%% # 0=成功
%% # 1=没有幸运星
%% # 3=背包已满
%% # >=127 = 程序异常
%% TODO: 背包已满
handle(13040, [], Rs) ->
	#role{id = Rid, name = Name, luck = {LuckStar, LuckDia, LuckUsed, ValSum}} = Rs,
	case LuckStar >= 1 of
		true ->
			LuckId = mod_luck:get_luck_id(),
			case data_luck:get(LuckId) of
				[] -> {ok, [1, 0, 0]};
				LuckData ->
					Rand = util:rand(1, 178000),
					Cumulative = data_config:get(starCumulative),
					case luck_process(Rand, LuckData) of
						{1, Num, Quality, Pos, Val} ->
							Rs1 = lib_role:add_attr(gold, Num, Rs),
							Luck1 = {LuckStar - 1, LuckDia + Cumulative, LuckUsed + 1, ValSum + Val},
							Rs2 = Rs1#role{luck = Luck1},
							lib_role:notice(gold, Rs2),
							lib_role:notice(luck, Rs2),
							{_, LuckDia1, LuckUsed1, ValSum1} = Rs2#role.luck,
							gen_server:cast(luck, {set_myrank, Rid, Name, LuckUsed1, ValSum1, 1, Num, Quality}),
							Rs3 = mod_attain:attain_state(46, 1, Rs2),
                            Rs0 = lib_role:add_attr_ok(gold, 9, Rs, Rs3),
							{ok, [0, LuckDia1, Pos], Rs0};
						{2, Num, Quality, Pos, Val} ->
							Rs1 = lib_role:add_attr(diamond, Num, Rs),
							Luck1 = {LuckStar - 1, LuckDia + Cumulative, LuckUsed + 1, ValSum + Val},
							Rs2 = Rs1#role{luck = Luck1},
							lib_role:notice(luck, Rs2),
							lib_role:notice(diamond, Rs2),
							{_, LuckDia1, LuckUsed1, ValSum1} = Rs2#role.luck,
							gen_server:cast(luck, {set_myrank, Rid, Name, LuckUsed1, ValSum1, 2, Num, Quality}),
							Rs3 = mod_attain:attain_state(46, 1, Rs2),
                            Rs0 = lib_role:add_attr_ok(gold, 9, Rs, Rs3),
							{ok, [0, LuckDia1, Pos], Rs0};
						{3, Num, Quality, Pos, Val} ->
							Luck1 = {LuckStar - 1, LuckDia + Cumulative, LuckUsed + 1, ValSum + Val},
							Rs1 = Rs#role{luck = Luck1},
							Rs2 = luck_rebate(LuckDia + Cumulative, Rs1),
							lib_role:notice(luck, Rs2),
							{_, LuckDia1, LuckUsed1, ValSum1} = Rs2#role.luck,
							gen_server:cast(luck, {set_myrank, Rid, Name, LuckUsed1, ValSum1, 3, Num, Quality}),
							Rs0 = mod_attain:attain_state(46, 1, Rs2),
							{ok, [0, LuckDia1, Pos], Rs0};
						{RewardTid, Num, Quality, Pos, Val} ->
							case mod_item:add_item(Rs, RewardTid, Num) of
                                {ok, Rs1, PA, EA} ->
                                    mod_item:send_notice(Rs#role.pid_sender, PA, EA),
									Luck1 = {LuckStar - 1, LuckDia + Cumulative, LuckUsed + 1, ValSum + Val},
									Rs2 = Rs1#role{luck = Luck1},
									lib_role:notice(luck, Rs2),
									{_, LuckDia1, LuckUsed1, ValSum1} = Rs2#role.luck,
									gen_server:cast(luck, {set_myrank, Rid, Name, LuckUsed1, ValSum1, RewardTid, Num, Quality}),
									Rs0 = mod_attain:attain_state(46, 1, Rs2),
									{ok, [0, LuckDia1, Pos], Rs0#role{save = [items]}};
								%% TODO:邮件系统
								{error, full} ->
									{ok, [3, 0, 0]};
								{error, _} ->
									{ok, [127, 0, 0]}
							end
					end
			end;
		false ->
			{ok, [1, 0, 0]}
	end;

%% 幸运星实时排行榜
handle(13042, [], Rs) ->
    ?DEBUG("recent_list 13042", []),
	luck ! {recent_list, Rs#role.pid_sender},
    {ok};
%%
%% 幸运星总排行榜,本周排行
handle(13044, [1], Rs) ->
	?DEBUG("rank_list 13044", []),
    luck ! {rank_list, Rs#role.pid_sender},
    {ok};
handle(13044, [0], Rs) ->
    luck ! {week_rank_list, Rs#role.pid_sender},
    {ok};

%% 幸运星期号
handle(13045, [], Rs) ->
	{LuckStar, _, LuckUsed, _} = Rs#role.luck,
    {ok, [1, LuckStar, LuckUsed], Rs};

handle(_Cmd, _Data, _RoleState) ->
    {error, bad_request}.


%% === 私有函数 ===
%%
% timetest() ->
%     {M, S, _MS} = os:timestamp(),
%     M * 1000000 + S. % + MS / 1000000.

%% luck star
luck_process(Rand, [{Tid, Pos, Num, Quality, Min, Max, Value} | T]) ->
    case Rand >= Min andalso Rand =< Max of
        true -> {Tid, Num, Quality, Pos, Value};
        false -> luck_process(Rand, T)
    end;
luck_process(_, []) -> {0, 0, 0, 0, 0}.

-spec luck_rebate(LuckDia, Rs) -> NewRs when
	  LuckDia :: integer(),
	  Rs :: #role{},
	  NewRs :: #role{}.
luck_rebate(LuckDia, Rs) ->
	case LuckDia >= data_config:get(starBack) of
		true ->
			Rs1 = lib_role:add_attr(diamond, LuckDia, Rs),
			lib_role:notice(diamond, Rs1),
			{LuckStar1, _LuckDia1, LuckUsed1, ValSum1} = Rs1#role.luck,
			Lists1 = {LuckStar1, 0, LuckUsed1, ValSum1},
			Rs2 = Rs1#role{luck = Lists1},
			Rs0 = lib_role:add_attr_ok(diamond, 9, Rs, Rs2),
			Rs0;
		false ->
			Rs
	end.

jewel(Q, Rs) ->
    JewelData = data_jewel:get(Q),
    RateN = util:get_val(rate_next, JewelData),
    Rate  = util:get_val(rate, JewelData),
    Rate2 = util:get_val(rate2, JewelData),
    Money = util:get_val(money, JewelData),
    Num   = util:get_val(num, JewelData),
    {CType, RtCode} = case Money of
        1 -> {gold, 1};
        2 -> {diamond, 2}
    end,
    case lib_role:spend(CType, Num, Rs) of
        {ok, Rs1} ->
            case jewel1(Rate, Rate2, RateN, Q, Rs1) of
                {ok, NextQ, ItemId, Tid, Rs2} ->
                    Rs3 = lib_role:spend_ok(CType, 8, Rs, Rs2),
					lib_role:notice(Rs3),
                    {ok, [0, NextQ, ItemId, Tid], Rs3};
                {error, full} ->
                    {ok, [3, 0, 0, 0]};
                {error, Reason} ->
                    ?ERR("~w", [Reason]),
                    {ok, [130, 0, 0, 0]}
            end;
        {error, _} -> {ok, [RtCode, 0, 0, 0]}
    end.

jewel1(Rate, Rate2, RateN, Q, Rs) ->
    %% 状态处理
    Jewel = case Q > 1 of
        true ->
            %% 如果当前抽取的品质大于1，则把状态置0
            lists:keyreplace(Q, 1, Rs#role.jewel, {Q, 0});
        false ->
            %% 品质1的状态始终为1
            Rs#role.jewel
    end,
    {Jewel1, NextQ} = case util:rate(RateN) of
        true ->
            %% 成功开启下一品质等级
            J = lists:keyreplace(Q+1, 1, Jewel, {Q+1, 1}),
            {J, Q+1};
        false ->
            %% 开启下一品质等级失败，则掉回品质1
            {Jewel, 1}
    end,
    %% 抽取宝珠
    {Tid, ObtainQ} = case util:rate(Rate) of
        true ->
            %% 成功抽取到当前等级的宝珠
            {get_jewel_id(Q), Q};
        false ->
            %% 没有抽取到当前等级的宝珠
            %% 计算是否能抽取到其它等级的宝珠
            case util:rate(Rate2) of
                true ->
                    %% 成功抽取到其它等级的宝珠
                    QQ = case Q > 1 of
                        true -> util:rand(1, Q-1);
                        false -> 1
                    end,
                    {get_jewel_id(QQ), QQ};
                false ->
                    {0, 0}
            end
    end,
    case Tid > 0 of
        true ->
            %% 抽取到了宝珠，添加物品
            case mod_item:add_item(Rs, Tid, 1) of
                {ok, Rs1, PA, EA} ->
                    mod_item:send_notice(Rs#role.pid_sender, PA, EA),
                    %% 成就推送
                    Rs2 = case ObtainQ of
                        3 -> mod_attain:attain_state(24, 1, Rs1);
                        4 -> mod_attain:attain_state(25, 1, Rs1);
                        5 -> mod_attain:attain_state(26, 1, Rs1);
                        _ -> Rs1
                    end,
                    Rs0 = case ObtainQ =:= 4 of
                        true -> mod_attain:attain_state(53, 1, Rs2);
                        false -> Rs2
                    end,
                    [#item{id = ItemId} | _] = EA ++ PA,
					{ok, NextQ, ItemId, Tid, Rs0#role{save = [items], jewel = Jewel1}};
				%% TODO:邮件系统
				{error, full} ->
					{error, full};
				{error, Error} ->
                    {error, Error}
            end;
        false ->
            {ok, NextQ, 0, 0, Rs#role{jewel = Jewel1}}
    end.

get_jewel_id(Quality) ->
    Ids = data_prop:get(ids),
    {Type} = util:get_range_data(data_jewel2),
    get_jewel_id1(Ids, Quality, Type).

get_jewel_id1([Tid | Ids], Q, T) ->
    Data = data_prop:get(Tid),
    Quality = util:get_val(quality, Data),
    Sort = util:get_val(sort, Data),
    Control1 = util:get_val(control1, Data),
    case Sort == 4 andalso Quality == Q
        andalso Control1 == T of
        true -> Tid;
        false -> get_jewel_id1(Ids, Q, T)
    end;
get_jewel_id1([], _, _) ->
    ?WARN("get_jewel_id1", []),
    0.

%% ----------------------

get_produce_ids([H | T], Rt) ->
    Rt1 = Rt ++ get_produce_ids1(H),
    get_produce_ids(T, Rt1);
get_produce_ids([], Rt) ->
    Rt.

%% {Type,Num,ID}
%%
%% 掉落类型(Type)：
%% 1一次性抽取Num个，ID不会重复；
%% 2为Num个独立事件，每次抽取1个ID，最终抽取的ID可能会重复.
%%
%% 以上两种类型中，抽取到的掉落ID不代表一定会有物品，这要取决于掉落ID对应的物品ID中是否有0存在。
get_produce_ids1({Type, Num, Id}) ->
    case data_produce:get(Id) of
        undefined ->
            ?WARN("Error Produce Id: ~w", [Id]),
            [];
        ProduceData ->
            {_, {_, Top}} = lists:last(ProduceData),
            case Type of
                1 -> get_produce_ids21(Num, Top, ProduceData, []);
                2 -> get_produce_ids22(Num, Top, ProduceData, [])
            end
    end.

get_produce_ids21(0, _Top, _Range, Rt) -> Rt;
get_produce_ids21(Num, Top, Range, Rt) ->
    %% ?DEBUG("Range:~w", [Range]),
    Rand = util:rand(1, Top),
    {Rt1, Range1} = case [{N, {S, E}} || {N, {S, E}} <- Range, S =< Rand, E >= Rand] of
        [{0, X1}] -> {[], lists:delete({0, X1}, Range)};
        [{Tid, X1}] -> {[Tid], lists:delete({Tid, X1}, Range)};
        [] -> {[], Range}
    end,
    Num1 = Num - 1,
    {Range2, Top1} = case Num1 > 0 andalso Range1 =/= [] of
                         true ->
                             Range11 = fix_range(Range1, 1, []),
                             {_, {_, Top11}} = lists:last(Range11),
                             {Range11, Top11};
                         false ->
                             {Range1, Top}
                     end,
    get_produce_ids21(Num1, Top1, Range2, Rt1 ++ Rt).

get_produce_ids22(0, _Top, _Range, Rt) -> Rt;
get_produce_ids22(Num, Top, Range, Rt) ->
    Rand = util:rand(1, Top),
    case [N || {N, {S, E}} <- Range, S =< Rand, E >= Rand] of
        [0] -> get_produce_ids22(Num-1, Top, Range, Rt);
        [Tid] -> get_produce_ids22(Num-1, Top, Range, [Tid|Rt]);
        [] ->
            ?WARN("get_produce_ids22", []),
            get_produce_ids22(Num-1, Top, Range, Rt)
    end.

%% [{101001,{1,50}},{101000,{51,70}},{101001,{71,100}},{101002,{1,48}},{0,{49,98}}]
fix_range([{Tid, {S, E}} | T], Fix, Rt) ->
    {Fix1, Elem} = case S == Fix of
                       true ->
                           {E + 1, {Tid, {S, E}}};
                       false ->
                           Range = E - S,
                           S1 = Fix,
                           E1 = S1 + Range,
                           {E1 + 1, {Tid, {S1, E1}}}
                   end,
    Rt1 = [Elem | Rt],
    fix_range(T, Fix1, Rt1);
fix_range([], _, Rt) ->
    lists:reverse(Rt).

produce_item(Rs) ->
    case data_tollgate:get(Rs#role.produce_pass) of
    %% case data_tollgate:get({1, util:rand(1,2)}) of
        undefined ->
            ?WARN("Error pass: ~w", [Rs#role.produce_pass]),
            {error, no_pass};
        Data ->
            Produce = util:get_val(produce, Data),
            Ids = get_produce_ids(Produce, []),
            mod_item:add_items(Rs#role{produce_pass = undefined}, Ids)
    end.

%% gen_range(Data) ->
%%     gen_range(Data, 1, []).
%% gen_range([{X1, X2, W} | T], Fix, Rt) ->
%%     S = Fix,
%%     E = S + W - 1,
%%     Rt1 = [{X1, X2, S, E} | Rt],
%%     gen_range(T, E + 1, Rt1);
%% gen_range([], _, Rt) ->
%%     lists:reverse(Rt).
%%
%% gen_item_tid(Num, Range) ->
%%     {_, _, _, Top} = lists:last(Range),
%%     gen_item_tid(Num, Top, Range, []).
%%
%% gen_item_tid(0, _Top, _Range, Rt) ->
%%     Rt;
%% gen_item_tid(Num, Top, Range, Rt) ->
%%     Rand = util:rand(1, Top),
%%     case [{N, C} || {N, C, S, E} <- Range, S =< Rand, E >= Rand] of
%%         [{N, C}] -> gen_item_tid(Num-1, Top, Range, [{N, C}|Rt]);
%%         [] ->
%%             ?WARN("gen_item_tid", []),
%%             gen_item_tid(Num-1, Top, Range, Rt)
%%     end.
