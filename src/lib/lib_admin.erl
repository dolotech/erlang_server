%%----------------------------------------------------
%% Admin
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(lib_admin).
-export(
   [
    get_online_roleid/0
    ,get_online_roleid/1
    ,add_item/3
    ,add_item/4
    ,add_gold/2
    ,add_diamond/2
    ,add_item_by_sort/3
    ,add_attr/3
    ,add_attr/4
   ]
  ).

-include("common.hrl").
-include("offline.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

add_gold(Rid, Num) ->
    Pid = lib_role:get_role_pid(role_id, Rid),
    Pid ! {pt, 2005, [Num]}.

add_diamond(Rid, Num) ->
    Pid = lib_role:get_role_pid(role_id, Rid),
    Pid ! {pt, 2006, [Num]}.

add_item_by_sort(Rid, Sort, Num) ->
    Ids = mod_item:get_ids_by_sort(Sort),
    IdsTL = [{Id, Num} || Id <- Ids],
    Pid = lib_role:get_role_pid(role_id, Rid),
    Pid ! {pt, 2002, [IdsTL]}.

add_item(Rid, Tid, Num) ->
    Pid = lib_role:get_role_pid(role_id, Rid),
    Pid ! {pt, 2002, [[{Tid, Num}]]}.

add_item(IdType, Id, Tid, Num) ->
    case get_pid(IdType, Id) of
        false -> {error};
        Pid ->
            Ids = case Tid < 1000 of
                true -> get_tids(Tid, Num);
                false -> 
                    AllIds = data_equ:get(ids) ++
                    data_prop:get(ids),
                    case lists:member(Tid, AllIds) of
                        true -> [{Tid, Num}];
                        false -> []
                    end
            end,
            case Ids == [] of
                true -> {error};
                false -> 
                    Pid ! {pt, 2002, [Ids]},
                    {ok, Tid, Num}
            end
    end.

get_tids(Sort, Num) ->
    Ids = mod_item:get_ids_by_sort(Sort),
    [{Id, Num} || Id <- Ids].

add_attr(IdType, Id, AttrName, AttrVal) ->
    case get_pid(IdType, Id) of
        false -> {error};
        Pid ->
            Pid ! {pt, 2008, [AttrName, AttrVal]},
            {ok, AttrName, AttrVal}
    end.

get_pid(IdType, Id) ->
    Id2 = fix_id(IdType, Id),
    lib_role:get_role_pid(IdType, Id2).

fix_id(name, Id) when is_list(Id) -> list_to_bitstring(Id);
fix_id(account_id, Id) when is_list(Id) -> list_to_bitstring(Id);
fix_id(role_id, Id) when is_list(Id) -> list_to_integer(Id);
fix_id(_, Id) -> Id.


add_attr(Rid, Attr, Num) ->
    Pid = lib_role:get_role_pid(role_id, Rid),
    Pid ! {pt, 2008, [Attr, Num]}.

get_online_roleid()->
    MS = ets:fun2ms(
    fun(#online{id=Id})-> Id end
    ),
    L = ets:select(online, MS),
    Rt = [ integer_to_list(Rid) || Rid <- L ], % 这里不能直接返回，不然ei_rpc无法解包
    Rt.

get_online_roleid(list)->
    MS = ets:fun2ms(
    fun(#online{id=Id})-> Id end
    ),
    ets:select(online, MS).
