%%----------------------------------------------------
%% 环境变量
%%
%% Usage: myenv:set(Key, Val) -> env:get(Key)
%% 
%% @author rolong@vip.qq.com
%%----------------------------------------------------
-module(myenv).
-include("common.hrl").
-export([
        init/0
        ,set/2
        ,del/1
        ,show/0
    ]).

init() ->
    ?INFO("init env ..."),
    ets:new(myenv, [{keypos, 1}, named_table, public, set]),
    compile([]),
    ?INFO("Usage: myenv:set(Key, Val). env:get(Key)."),
    ok.

set(K, V) ->
    L = application:get_all_env(myserver),
    case lists:keymember(K, 1, L) of
        true -> {error, existed_in_application_env};
        false ->
            ets:insert(myenv, {K, V}),
            Kvs = ets:tab2list(myenv),
            compile(Kvs)
    end.

del(K) ->
    ets:delete(myenv, K),
    Kvs = ets:tab2list(myenv),
    compile(Kvs).

show() ->
    Kvs = ets:tab2list(myenv),
    io:format("~n----------------- key->val -----------------~n"),
    io:format("~w~n", [Kvs]),
    Src = get_src(Kvs),
    io:format("~n-----------------code start-----------------~n"),
    io:format("~s", [Src]),
    io:format("~n----------------- code end -----------------~n").

compile(Kvs) ->
    try
        Src = get_src(Kvs),
        {Mod, Code} = dynamic_compile:from_string(Src),
        code:load_binary(Mod, "env.erl", Code)
    catch
        T : X -> ?WARNING("Error when compiling env! [~p:~p]", [T, X])
    end.

get_src(Kvs) ->
    Kvs1 = lists:map(fun({K, V})->
                io_lib:format("get(~p) -> ~p;\n", [K, V])
        end, Kvs),
    Kvs2 = lists:flatten(Kvs1),
    {H, T} = get_init_src(),
    H ++ Kvs2 ++ T.

get_init_src() ->
    L = application:get_all_env(myserver),
    Fun = lists:map(fun({K, V})->
                io_lib:format("get(~w) -> ~w;\n", [K, V])
        end, L),
    SrcHead = "-module(env).\n-export([get/1]).\n\n" ++ lists:flatten(Fun),
    SrcTail = "get(_) -> undefined.\n",
    {SrcHead, SrcTail}.
