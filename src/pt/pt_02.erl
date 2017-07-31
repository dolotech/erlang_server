%%----------------------------------------------------
%% 协议02 - 游戏事件
%%
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-module(pt_02).
-export([handle/3]).

-include("common.hrl").

%% 获得英雄
handle(2001, [], Rs) ->
    Rs1 = mod_hero:add_hero(Rs, rand),
    ?INFO("+hero!"),
    {ok, Rs1};

%% 获得物品
handle(2002, [Ids], Rs) ->
    ?DEBUG("~s, Add Item:~w", [Rs#role.account_id, Ids]),
    case mod_item:add_items(Rs, Ids) of
        {ok, Rs2, PA, EA} ->
            case is_pid(Rs#role.pid_sender) of
                true -> mod_item:send_notice(Rs#role.pid_sender, PA, EA);
                false -> ok
            end,
            {ok, Rs2#role{save = [role, items]}};
        {error, Reason} ->
            ?WARN("Add Item:~w", [Reason]),
            {ok}
    end;

%% 保存英雄到数据库
handle(2003, [], Rs) ->
    Heroes = mod_hero:db_update(Rs#role.id,
                                Rs#role.heroes),
    {ok, Rs#role{heroes = Heroes}};

%% 增加金币
handle(2005, [Num], Rs) ->
    ?DEBUG("~s, Add Gold:~w", [Rs#role.account_id, Num]),
    Rs1 = lib_role:add_attr(gold, Num, Rs),
    case is_pid(Rs#role.pid_sender) of
        true -> lib_role:notice(Rs1);
        false -> ok
    end,
    Rs2 = lib_role:add_attr_ok(gold, 31, Rs, Rs1),
    {ok, Rs2};

%% 增加钻石
handle(2006, [Num], Rs) ->
    ?DEBUG("~s, Add Diamond:~w", [Rs#role.account_id, Num]),
    Rs1 = lib_role:add_attr(diamond, Num, Rs),
    case is_pid(Rs#role.pid_sender) of
        true -> lib_role:notice(Rs1);
        false -> ok
    end,
    Rs2 = lib_role:add_attr_ok(diamond, 32, Rs, Rs1),
    {ok, Rs2};

handle(2008, [Attr, Num], Rs) ->
    ?DEBUG("~s, Add ~w: ~w", [Rs#role.account_id, Attr, Num]),
    Rs1 = lib_role:add_attr(Attr, Num, Rs),
    case is_pid(Rs#role.pid_sender) of
        true -> lib_role:notice(Rs1);
        false -> ok
    end,
    {ok, Rs1#role{save = [role]}};

%% 英雄升级成就推送
handle(2010, [Lev], Rs) ->
	Rs1 = mod_attain:attain_state2(1, Lev, Rs),
    {ok, Rs1};

handle(_Cmd, _Data, _Rs) ->
    {error, bad_request}.

%% === 私有函数 ===

%% vim: set ft=erlang foldmethod=marker foldmarker=%%',%%.:
