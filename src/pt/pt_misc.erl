%%----------------------------------------------------
%% 协议10 - 综合
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-module(pt_misc).
-export([handle/3]).

-include("common.hrl").

handle(10005, [], Rs) ->
    admin ! {send_notice, Rs#role.pid_sender},
    {ok};

handle(10010, [Type, Content], Rs) when Type == 1 orelse Type == 2 ->
    Sql = "INSERT INTO `feedback` (`role_id`, `type`, `ctime`, `ip`, `aid`, `content`) "
    "VALUES (~s, ~s, ~s, ~s, ~s, ~s);",
    #role{id = Rid, ip = Ip, account_id = Aid} = Rs,
    case db:execute(Sql, [Rid, Type, util:unixtime(), Ip, Aid, Content]) of
        {ok, _} -> {ok, [0]};
        {error, Reason} ->
            ?WARN("feedback error: ~w", [Reason]),
            {ok, [1]}
    end;

handle(_Cmd, _Data, _Rs) ->
    {error, bad_request}.

%% === 私有函数 ===

%% vim: set ft=erlang foldmethod=marker foldmarker=%%',%%.:
