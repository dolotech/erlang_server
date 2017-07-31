%%----------------------------------------------------
%% 内部协议01 - 事件处理
%%
%% @author Rolong<erlang6@qq.com>
%%
%%----------------------------------------------------

-module(pt_01).
-export([handle/3]).

-include("common.hrl").

%% 事件处理

%% 反击战报
handle(1001, {lost_report, Rid, Name}, Rs) ->
    ?INFO("~w", [{add, lost_report, Rid, Name}]),
    Report = lists:sublist(Rs#role.arena_lost_report, 1, 49),
    LostReport = [{Rid, Name} | Report],
    Rs1 = Rs#role{arena_lost_report = LostReport},
    {ok, Rs1};

handle(1001, Event, Rs) ->
    ?WARN("undefined event, Rid:~w, Event:~w", [Rs#role.id, Event]),
    {ok};

%% 登陆完成后处理离线事件
handle(1002, Events, Rs) ->
    %% ?INFO("do_events:~w", [Events]),
    Events1 = lists:reverse(Events),
    do_events(Rs, Events1),
    {ok};

%% 服务器启动后第一次登陆
handle(1003, [], _Rs) ->
    {ok};

handle(1005, [], Rs) ->
    ?INFO("Role State:~p", [Rs]),
    {ok};

handle(_Cmd, _Data, _RoleState) ->
    {error, bad_request}.

%%' === 私有函数 ===

do_events(Rs, [Event | Events]) ->
    self() ! {pt, 1001, Event},
    do_events(Rs, Events);
do_events(_Rs, []) -> ok.

%%.

%% vim: set foldmethod=marker foldmarker=%%',%%.:
