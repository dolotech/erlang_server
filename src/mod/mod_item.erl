%%----------------------------------------------------
%% $Id: mod_item.erl 5772 2013-12-07 09:34:35Z rolong $
%%
%% 物品模块
%%----------------------------------------------------

-module(mod_item).
-export([
    %% get_equ_ids/0
    init_item/2
    ,init_item/3
    ,init_items/2
    ,get_item/2
    ,set_item/2
    ,del_item/4
    ,del_items/3
    ,del_items_from_db/2
    ,add_item/3
    ,add_items/2
    ,db_init/1
    ,db_update/2
    ,db_insert/2
    ,db_delete/2
    %% ,check_sorts/3
    %% ,check_sorts/2
    %% ,check_sort/3
    %% ,check_sort/2
    ,get_strengthen_rest_time/1
    ,strengthen/5
    ,get_ids_by_sort/1
    ,embed/2
    ,send_notice/2
    ,send_notice/3
    ,pack_prop/1
    ,pack_equ/1
    ,count_unit/2
]).

-include("common.hrl").
-include("prop.hrl").
-include("equ.hrl").

-define(ITEM_ZIP_VERSION, 1).

%% ------------------------------------------------------------------
%% EUNIT TEST
%% ------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(TEST).

data_equ_test() ->
    ?assert(undefined == data_equ:get(0)),
    Ids = data_equ:get(ids),
    ?assert(length(Ids) > 0),
    lists:foreach(fun(Id) ->
                Data = data_equ:get(Id),
                ?assert(util:get_val(sort, Data, 0) > 0)
        end, Ids),
    ok.

data_prop_test() ->
    ?assert(undefined == data_prop:get(0)),
    Ids = data_prop:get(ids),
    ?assert(length(Ids) > 0),
    lists:foreach(fun(Id) ->
                Data = data_prop:get(Id),
                Tab = util:get_val(tab, Data, 0),
                ?assert(lists:member(Tab, [1, 2])),
                ?assert(util:get_val(sort, Data, 0) > 0)
        end, Ids),
    ok.

-endif.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% 计算物品单元格
-spec count_unit(Tab, Items) -> Count when
    Tab :: ?TAB_EQU | ?TAB_PROP | ?TAB_MAT,
    Items :: [#item{}],
    Count :: integer().

count_unit(Tab, Items) ->
    F = fun
        (#item{tab = T}, Count) when Tab =:= T -> Count + 1;
        (I, Count) ->
                case lists:member(I#item.tab, [?TAB_EQU, ?TAB_PROP, ?TAB_MAT]) of
                    true -> ok;
                    false -> ?WARN("Tab:~w, Item:~w", [I#item.tab, I])
                end,
                Count
    end,
    lists:foldl(F, 0, Items).

%% 物品删除通知
send_notice(Id, Dels) ->
    sender:pack_send(Id, 13030, [Dels]).

%% 物品更新通知
send_notice(Id, Prop, Equ) ->
    Data1 = [mod_item:pack_equ(X) || X <- Equ],
    Data2 = [mod_item:pack_prop(X) || X <- Prop],
    sender:pack_send(Id, 13001, [3, Data1, Data2]).

pack_prop(#item{id = Id, tid = Tid, attr = Attr}) ->
    #prop{num = Num} = Attr,
    [Id, Tid, Num].

pack_equ(#item{id = Id, tid = Tid, attr = Attr}) ->
    #equ{
       hero_id    = HeroId
       ,lev       = Lev
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
       ,embed = Embed
      } = Attr,
    EmbedData = [[X1, X2] || {X1, _, X2} <- Embed],
    [Id, Tid
     ,HeroId
     ,Lev
     ,EmbedData
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

get_ids_by_sort(Sort) when is_integer(Sort) ->
    get_ids_by_sort([Sort]);

get_ids_by_sort(Sorts) ->
    Ids = data_prop:get(ids),
    get_ids_by_sort(Sorts, Ids, []).

get_ids_by_sort(Sorts, [Tid | Ids], Rt) ->
    Data = data_prop:get(Tid),
    case lists:member(util:get_val(sort, Data, 0), Sorts) of
        true -> get_ids_by_sort(Sorts, Ids, [Tid | Rt]);
        false -> get_ids_by_sort(Sorts, Ids, Rt)
    end;
get_ids_by_sort(_Sorts, [], Rt) ->
    Rt.

%% get_equ_ids() ->
%%     Ids = data_item:get(ids),
%%     get_equ_ids1(Ids, []).
%%
%% get_equ_ids1([Tid | Ids], Rt) ->
%%     Item = data_item:get(Tid),
%%     case lists:member(util:get_val(sort, Item, 0),
%%             [2, 3, 4, 5]) of
%%         true -> get_equ_ids1(Ids, [Tid | Rt]);
%%         false -> get_equ_ids1(Ids, Rt)
%%     end;
%% get_equ_ids1([], Rt) ->
%%     Rt.

%%' init_item
init_items(Rs, Ids) ->
    init_items(Rs, Ids, []).

init_items(Rs, [Tid | Ids], Rt) ->
    {ok, Id, Rs1} = lib_role:get_new_id(item, Rs),
    Item = init_item(Id, Tid, 1),
    Rt1 = [Item | Rt],
    init_items(Rs1, Ids, Rt1);
init_items(Rs, [], Rt) ->
    Rs#role{items = Rt ++ Rs#role.items}.

init_item(Id, Tid) ->
    init_item(Id, Tid, 1).

init_item(Id, Tid, Num) ->
    case Tid >= ?MIN_EQU_ID of
        true -> init_equ(Id, Tid, data_equ:get(Tid));
        false -> init_prop(Id, Tid, Num, data_prop:get(Tid))
    end.

equ_attr1(def      , V, Equ) -> Equ#equ{def       = V};
equ_attr1(pun      , V, Equ) -> Equ#equ{pun       = V};
equ_attr1(hit      , V, Equ) -> Equ#equ{hit       = V};
equ_attr1(dod      , V, Equ) -> Equ#equ{dod       = V};
equ_attr1(crit     , V, Equ) -> Equ#equ{crit      = V};
equ_attr1(crit_num , V, Equ) -> Equ#equ{crit_num  = V};
equ_attr1(crit_anti, V, Equ) -> Equ#equ{crit_anti = V};
equ_attr1(Else     , V, Equ) ->
    ?WARN("undefined equ_attr: ~w, value: ~w", [Else, V]),
    Equ.

equ_attr(Num, Attrs, Data, Equ) when Num > 0 ->
    Num1 = Num - 1,
    Attr = util:rand_element(Attrs),
    Attrs1 = lists:delete(Attr, Attrs),
    V = attr_val(Attr, Data),
    Equ1 = equ_attr1(Attr, V, Equ),
    equ_attr(Num1, Attrs1, Data, Equ1);
equ_attr(_, _, _, Equ) ->
    Equ.

%% 品质
%% 1=0个
%% 2=1个
%% 3=2个
%% 4=3个
%% 5=4个

get_attr_num(1) -> 0;
get_attr_num(2) -> 1;
get_attr_num(3) -> 2;
get_attr_num(4) -> 3;
get_attr_num(5) -> 4;
get_attr_num(_) -> 0.

attr_val(Attr, Data) ->
    V = util:get_val(Attr, Data, 0),
    %% Rand1 = util:rand(1, 100),
    %% Rand2 = util:rand(1, 100),
    %% util:ceil((Rand1*Rand2/10000*0.4*V+0.6*V)).
    util:ceil(V * data_fun:equ_offset()).

init_equ(Id, Tid, Data) ->
    Attrs = [def, pun, hit, dod, crit, crit_num, crit_anti],
    Tou = util:get_val(tou, Data, 0),
    AddTou = case Tou > 0 of
        true -> 1;
        false -> 0
    end,
    Sort     = util:get_val(sort   , Data),
    Hp       = attr_val(hp, Data),
    Atk      = attr_val(atk, Data),
    Sockets  = util:get_val(sockets, Data),
    Quality  = util:get_val(quality, Data),
    %% 如果有韧性(tou)，则一定加上，随机属性数量减1
    AttrNum  = get_attr_num(Quality) - AddTou,
    Equ = #equ{
        quality    = Quality
        ,hp        = Hp
        ,atk       = Atk
        ,sockets   = Sockets
    },
    Equ1 = equ_attr(AttrNum, Attrs, Data, Equ),
    Equ2 = case AddTou of
        1 -> Equ1#equ{tou = Tou};
        0 -> Equ1
    end,
    #item{
        id = Id
        ,tid = Tid
        ,sort = Sort
        ,tab = ?TAB_EQU
        ,changed = 1
        ,attr = Equ2
    }.

init_prop(Id, Tid, Num, Data) ->
    Sort= util:get_val(sort   , Data),
    Max = util:get_val(num_max, Data),
    Q = util:get_val(quality, Data, 0),
    Tab  = util:get_val(tab    , Data),
    Prop = #prop{
        num = util:fix_max(Num, Max)
        ,quality = Q
    },
    #item{
        id = Id
        ,tid = Tid
        ,sort = Sort
        ,tab = Tab
        ,changed = 1
        ,attr = Prop
    }.

%%.

%%' 添加物品
add_items(Rs, Ids) ->
    add_items(Rs, Ids, [], []).
add_items(Rs, [{Tid, Num} | T], PropAdds, EquAdds) ->
    case add_item(Rs, Tid, Num, PropAdds, EquAdds) of
        {ok, Rs1, PropAdds1, EquAdds1} ->
            add_items(Rs1, T, PropAdds1, EquAdds1);
        {error, Reason} -> {error, Reason}
    end;
add_items(Rs, [Tid | T], PropAdds, EquAdds) ->
    add_items(Rs, [{Tid, 1} | T], PropAdds, EquAdds);
add_items(Rs, [], PropAdds, EquAdds) ->
    {ok, Rs, PropAdds, EquAdds}.

-spec add_item(Rs, Tid, Num) ->
    {ok, NewRs, PropAdds, EquAdds} | {error, Error} when
    Rs :: NewRs,
    NewRs :: #role{},
    Tid :: integer(),
    Num :: integer(),
    PropAdds :: EquAdds,
    EquAdds :: [#item{}],
    Error :: full | incorrect_num | undef.

add_item(Rs, Tid, Num) ->
    add_item(Rs, Tid, Num, [], []).
add_item(Rs, Tid, Num, PA, EA) ->
    Items = Rs#role.items,
    %% TODO: bag full
    if
        Num =< 0 ->
            {error, incorrect_num};
        Tid >= ?MIN_EQU_ID ->
            case count_unit(?TAB_EQU, Items) < Rs#role.bag_equ_max of
                true ->
                    {ok, Id, Rs1} = lib_role:get_new_id(item, Rs),
                    Data = data_equ:get(Tid),
                    Item = init_equ(Id, Tid, Data),
                    EA1 = [Item | EA],
                    {ok, Rs1#role{items = [Item | Items]}, PA, EA1};
                false ->
					?DEBUG("BAG_EQU:~w", [count_unit(?TAB_EQU, Items)]),
                    {error, full}
            end;
        true ->
            Data = data_prop:get(Tid),
            Max = util:get_val(num_max, Data, 1),
            %% 查找是否有可堆叠物品
            Finder = [ItemExist1 ||
                #item{tid = Tid1, attr = P} = ItemExist1 <- Items,
                Tid1 == Tid, P#prop.num < Max],
            case Finder of
                [] ->
                    {ok, Id, Rs1} = lib_role:get_new_id(item, Rs),
                    case Num =< Max of
                        true ->
                            Item = init_prop(Id, Tid, Num, Data),
                            PA1 = [Item | PA],
                            Items1 = [Item | Items],
                            case count_unit(?TAB_PROP, Items1) > Rs#role.bag_prop_max
                                orelse count_unit(?TAB_MAT, Items1) > Rs#role.bag_mat_max of
                                true ->
									?DEBUG("BAG_PROP:~w, BAG_Mat:~w", [count_unit(?TAB_PROP, Items1), count_unit(?TAB_MAT, Items1)]),
									{error, full};
                                false -> {ok, Rs1#role{items = Items1}, PA1, EA}
                            end;
                        false ->
                            Num1 = Num - Max,
                            Item = init_prop(Id, Tid, Max, Data),
                            Rs2 = Rs1#role{items = [Item | Items]},
                            PA1 = [Item | PA],
                            add_item(Rs2, Tid, Num1, PA1, EA)
                    end;
                [Item | Os] ->
                    case Os of
                        [] -> ok;
                        _ ->
                            [O | _] = Os,
                            ?WARN("Item(~w), OtherItems when add_item: ~w~n Item:~w~nOs  :~w",
                                [Item#item.attr#prop.num, O#item.attr#prop.num, Item, Os])
                    end,
                    #item{attr = Prop} = Item,
                    #prop{num = Num1} = Prop,
                    RestNum = Max - Num1,
                    case Num =< RestNum of
                        true ->
                            NewNum = Num1 + Num,
                            Prop1 = Prop#prop{num = NewNum},
                            Item1 = Item#item{attr = Prop1},
                            Items2 = set_item(Item1, Items),
                            PA1 = [Item1 | PA],
                            case count_unit(?TAB_PROP, Items2) > Rs#role.bag_prop_max
                                orelse count_unit(?TAB_MAT, Items2) > Rs#role.bag_mat_max of
                                true ->
									?DEBUG("BAG_PROP:~w, BAG_Mat:~w", [count_unit(?TAB_PROP, Items2), count_unit(?TAB_MAT, Items2)]),
									{error, full};
                                false -> {ok, Rs#role{items = Items2}, PA1, EA}
                            end;
                        false ->
                            Prop1 = Prop#prop{num = Max},
                            Item1 = Item#item{attr = Prop1},
                            Items2 = set_item(Item1, Items),
                            Num2 = Num - RestNum,
                            Rs1 = Rs#role{items = Items2},
                            PA1 = [Item1 | PA],
                            add_item(Rs1, Tid, Num2, PA1, EA)
                    end;
                _Undef ->
                    ?ERR("Error when add_item: ~w", [_Undef]),
                    {error, undef}
            end
    end.
%%.

get_item(Id, MyItems) ->
    lists:keyfind(Id, #item.id, MyItems).

set_item(MyItem, MyItems) ->
    lists:keystore(MyItem#item.id, #item.id, MyItems, MyItem).

%% get_gem_attr([]) -> {0, 0};
%% get_gem_attr([Tid]) -> get_gem_attr(Tid);
%% get_gem_attr(Tid) ->
%%     case data_item:get(Tid) of
%%         undefined ->
%%             {0, 0};
%%         GemData ->
%%             Atk = util:get_val(atk, GemData, 0),
%%             Hp = util:get_val(hp, GemData, 0),
%%             {Atk, Hp}
%%     end.

%% 镶嵌
%% 当SORT为4时
%% 1,攻击
%% 2,血量
%% 3,防御
%% 4,穿刺
%% 5,命中
%% 6,闪避
%% 7,暴击
%% 8,暴强
embed(EquItem, GemItem) ->
    #item{tid = _EquTid, attr = Equ} = EquItem,
    #item{tid = GemTid, attr = Prop} = GemItem,
    %% _EquData = data_prop:get(EquTid),
    GemData = data_prop:get(GemTid),
    AttrId = util:get_val(control1, GemData, 0),
    Control2 = util:get_val(control2, GemData, 0),
    AttrVal = util:ceil(Control2 * data_fun:jewel_offset()),
    #equ{embed = Embed, sockets = Sockets} = Equ,
    #prop{quality = GemQ} = Prop,
    RestSocket = Sockets - length(Embed),
    Member = lists:keyfind(AttrId, 2, Embed),
    New = {GemTid, AttrId, AttrVal},
    if
        AttrId == 0; AttrVal == 0 ->
            ?ERR("AttrId:~w, AttrVal:~w", [AttrId, AttrVal]),
            {error, error};
        Member =/= false ->
            {OldGemTid, _, _} = Member,
            OldGemData = data_prop:get(OldGemTid),
            OldQ = util:get_val(quality, OldGemData),
            case GemQ >= OldQ of
                true ->
                    Embed1 = lists:delete(Member, Embed),
                    Embed2 = [New | Embed1],
                    Equ1 = Equ#equ{embed = Embed2},
                    EquItem1 = EquItem#item{attr = Equ1},
                    {ok, EquItem1};
                false ->
                    ?WARN("error quality", []),
                    {error, quality}
            end;
        RestSocket =< 0 ->
            XX1 = util:rand_element(Embed),
            Embed1 = lists:delete(XX1, Embed),
            Embed2 = [New | Embed1],
            Equ1 = Equ#equ{embed = Embed2},
            EquItem1 = EquItem#item{attr = Equ1},
            {ok, EquItem1};
        true ->
            Embed1 = [New | Embed],
            Equ1 = Equ#equ{embed = Embed1},
            EquItem1 = EquItem#item{attr = Equ1},
            {ok, EquItem1}
    end.
    %% case (util:get_val(ctl1, EquData, 0) - length(CL1)) > 0 of
    %%     true ->
    %%         case util:rate(Rate) of
    %%             true ->
    %%                 #item{tid = Tid, sort = Sort, hp = Hp, atk = Atk} = GemItem,
    %%                 {AtkBase, HpBase} = get_gem_attr(Tid),
    %%                 {Atk2, Hp2} = get_gem_attr(CL -- CL1),
    %%                 {AtkAdd, HpAdd} = case Sort of
    %%                     41 -> {AtkBase - Atk2, HpBase - Hp2};
    %%                     42 ->
    %%                         {
    %%                             util:ceil(Atk * (AtkBase / 100 + 1))
    %%                             ,util:ceil(Hp * (HpBase / 100 + 1))
    %%                         };
    %%                     _Sort ->
    %%                         ?ERR("Error sort when embed:~w", [_Sort]),
    %%                         {AtkBase - Atk2, HpBase - Hp2}
    %%                 end,
    %%                 EquItem1 = EquItem#item{hp = EquHp + HpAdd,
    %%                     atk = EquAtk + AtkAdd,
    %%                     custom_list = [Tid | CL1]
    %%                 },
    %%                 {ok, 0, EquItem1};
    %%             false ->
    %%                 {ok, 1, EquItem}
    %%         end;
    %%     false ->
    %%         {error, no_sock}
    %% end.

%% 强化
strengthen(Item, Rate, RateF, Rise, AddTime) ->
    #equ{lev = Lev, atk = Atk, hp = Hp} = Item#item.attr,
    case util:rate1000(Rate) of
        true ->
            Atk1 = util:ceil(Atk * Rise),
            Hp1 = util:ceil(Hp * Rise),
            ATime = util:unixtime() + AddTime,
            Equ = Item#item.attr#equ{lev = Lev + 1, atime = ATime, atk = Atk1, hp = Hp1},
            Item1 = Item#item{attr = Equ},
            {ok, 0, Item1};
        false ->
            case util:rate(RateF) of
                true ->
                    Lev1 = case Lev > 0 of
                        true -> Lev - 1;
                        false -> 0
                    end,
                    Equ1 = Item#item.attr#equ{lev = Lev1},
                    Item1 = Item#item{attr = Equ1},
                    {ok, 1, Item1};
                false ->
                    {ok, 1, Item}
            end
    end.

%% 检查类型(强化幸运石类型检查)
%% check_sorts(Sorts, Id, Items) when is_list(Sorts) ->
%%     case get_item(Id, Items) of
%%         false -> false;
%%         Item -> lists:member(Item#item.sort, Sorts)
%%     end;
%%
%% check_sorts(_Sort, [], _Items) -> true;
%% check_sorts(Sort, [Id | Ids], Items) ->
%%     case check_sort(Sort, Id, Items) of
%%         true -> check_sorts(Sort, Ids, Items);
%%         false -> false
%%     end.
%%
%% check_sorts(Sorts, Item) when is_list(Sorts) ->
%%     lists:member(Item#item.sort, Sorts).
%%
%% check_sort(Sort, Id, Items) ->
%%     case get_item(Id, Items) of
%%         false -> false;
%%         Item -> check_sort(Sort, Item)
%%     end.
%%
%% check_sort(Sort, #item{sort = Sort}) -> true;
%% check_sort(_, _) -> false.

get_strengthen_rest_time(Item) when is_record(Item, item) ->
    get_strengthen_rest_time(Item#item.attr#equ.atime);
get_strengthen_rest_time(0) -> 0;
get_strengthen_rest_time(ATime) ->
    ATime - util:unixtime().

%%' 删除物品

%% @spec () -> {ok, MyItems1} | {error, Reason}
del_items(by_id, Dels, MyItems) ->
    del_items(by_id, Dels, MyItems, []);

del_items(by_tid, Dels, MyItems) ->
    del_items(by_tid, Dels, MyItems, []).

del_items(by_id, [{Id, Num} | T], MyItems, AccDels) ->
    del_items(by_id, [[Id, Num] | T], MyItems, AccDels);
del_items(by_id, [[Id, Num] | T], MyItems, AccDels) when Id =< 0; Num =< 0 ->
    del_items(by_id, T, MyItems, AccDels);
del_items(by_id, [[Id, Num] | T], MyItems, AccDels) ->
    case del_item(by_id, Id, Num, MyItems) of
        {ok, MyItems1, Dels} ->
            del_items(by_id, T, MyItems1, Dels ++ AccDels);
        {error, Reason} ->
            {error, Reason}
    end;
del_items(by_id, [], MyItems, AccDels) ->
    {ok, MyItems, AccDels};

del_items(by_tid, [{Id, Num} | T], MyItems, AccDels) ->
    del_items(by_tid, [[Id, Num] | T], MyItems, AccDels);
del_items(by_tid, [[Id, Num] | T], MyItems, AccDels)
when Id =< 0; Num =< 0 ->
    del_items(by_tid, T, MyItems, AccDels);
del_items(by_tid, [[Id, Num] | T], MyItems, AccDels) ->
    case del_item(by_tid, Id, Num, MyItems) of
        {ok, MyItems1, Dels} ->
            del_items(by_tid, T, MyItems1, Dels ++ AccDels);
        {error, Reason} ->
            {error, Reason}
    end;
del_items(by_tid, [], MyItems, AccDels) ->
    {ok, MyItems, AccDels}.

%% @spec () -> {ok, MyItems1} | {error, Reason}
del_item(by_id, Id, Num, MyItems) ->
    case lists:keyfind(Id, 2, MyItems) of
        false -> {error, no_item};
        MyItem ->
            #item{tid = Tid, attr = Attr} = MyItem,
            case Tid >= ?MIN_EQU_ID of
                true ->
                    %% 删除装备
                    MyItems1 = lists:keydelete(Id, 2, MyItems),
                    {ok, MyItems1, [[Id, 0]]};
                false ->
                    %% 删除道具
                    Num1 = Attr#prop.num,
                    if
                        Num == Num1 ->
                            MyItems1 = lists:keydelete(Id, 2, MyItems),
                            {ok, MyItems1, [[Id, 0]]};
                        Num < Num1 ->
                            NewNum = Num1 - Num,
                            Prop1 = Attr#prop{num = NewNum},
                            MyItem1 = MyItem#item{attr = Prop1},
                            MyItems1 = lists:keyreplace(Id, 2, MyItems, MyItem1),
                            {ok, MyItems1, [[Id, NewNum]]};
                        Num > Num1 ->
                            %% ?INFO("删除数量不正确：~w ! ~w", [Num, MyItem#item.num]),
                            {error, no_item}
                    end
            end
    end;

%% @spec () -> {ok, MyItems1, Dels} | {error, Reason}
%% 只有道具才能用tid删除
del_item(by_tid, Tid, Num, MyItems) ->
    del_item(by_tid, Tid, Num, MyItems, []).
del_item(by_tid, Tid, Num, MyItems, Dels) ->
    %% case lists:keyfind(Tid, #item.tid, MyItems) of
    case get_min_item(Tid, MyItems) of
        false -> {error, no_item};
        MyItem ->
            #item{id = Id, tid = Tid, attr = Prop} = MyItem,
            #prop{num = Num1} = Prop,
            if
                Num == Num1 ->
                    MyItems1 = lists:keydelete(Id, 2, MyItems),
                    Dels1 = [[Id, 0] | Dels],
                    {ok, MyItems1, Dels1};
                Num < Num1 ->
                    NewNum = Num1 - Num,
                    Prop1 = Prop#prop{num = NewNum},
                    MyItem1 = MyItem#item{attr = Prop1},
                    MyItems1 = lists:keyreplace(Id, 2, MyItems, MyItem1),
                    Dels1 = [[Id, Num] | Dels],
                    {ok, MyItems1, Dels1};
                Num > Num1 ->
                    Num2 = Num - Num1,
                    MyItems1 = lists:keydelete(Id, 2, MyItems),
                    Dels1 = [[Id, 0] | Dels],
                    del_item(by_tid, Tid, Num2, MyItems1, Dels1)
            end
    end.

del_items_from_db(Rid, [[Id, 0] | T]) ->
    db_delete(Rid, Id),
    del_items_from_db(Rid, T);
del_items_from_db(Rid, [_ | T]) ->
    del_items_from_db(Rid, T);
del_items_from_db(_Rid, []) ->
    ok.


get_min_item(Tid, Items) ->
    Items1 = [I || I = #item{tid = Tid1} <- Items, Tid1 == Tid],
    case sort_num(Items1) of
        [] -> false;
        [H | _T] ->
            %% ?INFO("~n~p", [[H|_T]]),
            H
    end.

sort_num([]) -> [];
sort_num([RR | T]) ->
    sort_num([R || R <- T, R#item.attr#prop.num < RR#item.attr#prop.num])
     ++[RR] ++
    sort_num([R || R <- T, R#item.attr#prop.num >= RR#item.attr#prop.num]).
%%.

%%' 数据库相关操作

-spec db_init(Rs) -> {ok, NewRs} | {error, Reason} when
    Rs :: #role{},
    NewRs :: #role{},
    Reason :: term().

db_init(Rs) ->
    Rid = Rs#role.id,
    Sql = "select item_id, val from item where role_id = ~s",
    case db:get_all(Sql, [Rid]) of
        {ok, Rows} ->
            {MaxId, Items} = db_init1(Rows, []),
            {ok, Rs#role{items = Items, max_item_id = MaxId}};
        {error, null} ->
            {ok, Rs#role{items = []}};
        {error, Reason} ->
            {error, Reason}
    end.

db_init1([H | T], Result) ->
    db_init1(T, [unzip(H) | Result]);
db_init1([], Result) ->
    MaxId = lists:max([H#item.id || H <- Result]),
    {MaxId, Result}.

db_delete(Rid, Tid) when is_integer(Tid) ->
    Sql = list_to_binary([
            <<"delete from item where role_id = ">>
            ,integer_to_list(Rid),<<" and item_id = ">>
            ,integer_to_list(Tid)
        ]),
    db:execute(Sql);
db_delete(Rid, H) ->
    db_delete(Rid, H#item.id).

-spec db_insert(Rid, Item) -> {ok, Num} | {error, Reason} when
    Rid :: integer(),
    Item :: #item{},
    Num :: integer(), %% 影响行数
    Reason :: term().

db_insert(Rid, H) ->
    Val = ?ESC_SINGLE_QUOTES(zip(H)),
    RidL = integer_to_list(Rid),
    HidL = integer_to_list(H#item.id),
    Sql = list_to_binary([
            <<"SELECT count(*) FROM `item` WHERE role_id = ">>
            ,RidL,<<" and item_id = ">>,HidL
        ]),
    case db:get_one(Sql) of
        {error, null} ->
            Sql2 = list_to_binary([
                    <<"insert item (role_id, item_id, val) value (">>
                    ,RidL,<<",">>,HidL,<<",'">>,Val,<<"')">>
                ]),
            %% ?INFO("insert item(~w) to db!", [H#item.id]),
            db:execute(Sql2);
        {error, Reason} ->
            {error, Reason};
        {ok, Num} ->
            %% 记录已存在
            {ok, Num}
    end.

-spec db_update(Rid, Items) -> {true, NewItems} | {false, NewItems} when
    Rid :: integer(),
    Items :: NewItems,
    NewItems :: [#item{}].

db_update(Rid, HL) ->
    db_update(Rid, HL, []).

db_update(Rid, [H | T], Result) ->
    Result1 = [H#item{changed = 0} | Result],
    case H#item.changed of
        1 ->
            Val = ?ESC_SINGLE_QUOTES(zip(H)),
            Sql = list_to_binary([
                    <<"update item set val = '">>,Val,<<"' where ">>
                    ,<<"role_id = ">>,integer_to_list(Rid)
                    ,<<" and item_id = ">>,integer_to_list(H#item.id)
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
            ?ERR("Undefined #item.changed: ~w", [H#item.changed]),
            db_update(Rid, T, Result1)
    end;
db_update(_Rid, [], Result) -> {true, Result}.

unzip([Id, Val]) ->
    <<Version:8, Tid:32, Sort:8, Tab:8, Bin1/binary>> = Val,
    case Version of
        1 ->
            Attr = case Tid >= ?MIN_EQU_ID of
                true ->
                    <<HeroId:32, Etime:32, Atime:32, Lev:8,
                    Hp      :32,
                    Atk     :32,
                    Def     :32,
                    Pun     :32,
                    Hit     :32,
                    Dod     :32,
                    Crit:32,
                    CritNum :32,
                    CritAnit:32,
                    Tou     :32,
                    Sockets :8,
                    EmbedLen:16, Bin2/binary>> = Bin1,
                    F1 = fun({B_, R_}) ->
                            << X1_:32, X2_:8, X3_:32, RB_/binary >> = B_,
                            {RB_, [{X1_, X2_, X3_}|R_]}
                    end,
                    {_Bin3, Embed} = protocol_fun:for(EmbedLen, F1, {Bin2, []}),
                    #equ{
                        hero_id = HeroId
                        ,etime = Etime
                        ,atime = Atime
                        ,lev = Lev
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
                        ,sockets   = Sockets
                        ,embed = Embed
                    };
                false ->
                    <<Num:8, Quality:8>> = Bin1,
                    #prop{
                        num = Num
                        ,quality = Quality
                    }
            end,
            #item{
                id = Id
                ,tid = Tid
                ,sort = Sort
                ,tab = Tab
                ,changed = 0
                ,attr = Attr
            };
        _ ->
            ?ERR("undefined version: ~w", [Version]),
            undefined
    end.

zip(H) ->
    #item{tid = Tid, sort = Sort, tab = Tab, attr = Attr} = H,
    case Tid >= ?MIN_EQU_ID of
        true ->
            #equ{
                hero_id    = HeroId
                ,etime     = Etime
                ,atime     = Atime
                ,lev       = Lev
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
                ,sockets   = Sockets
                ,embed = Embed
            } = Attr,
            %% ?INFO("Embed:~w", [Embed]),
            EmbedLen = length(Embed),
            EmbedBin = list_to_binary([<<X1:32, X2:8, X3:32>> || {X1, X2, X3} <- Embed]),
            <<?ITEM_ZIP_VERSION:8, Tid:32, Sort:8, Tab:8,
            HeroId:32, Etime:32, Atime:32, Lev:8,
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
            Sockets :8,
            EmbedLen:16, EmbedBin/binary>>;
        false ->
            #prop{num = Num, quality = Quality} = Attr,
            <<?ITEM_ZIP_VERSION:8, Tid:32, Sort:8, Tab:8, Num:8, Quality:8>>
    end.
%%.

%% vim: set filetype=erlang foldmarker=%%',%%. foldmethod=marker:
