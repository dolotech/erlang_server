%%----------------------------------------------------
%% 性能测试工具
%%----------------------------------------------------
-module(tester).
-compile(export_all).

%% 运行一个测试集
run(N, List) ->
    [[L, T1, T2] = H | T] = [timer(Label, F, N) || {Label, F} <- List],
    io:format("========================================================================~n"),
    io:format("~-20s = ~9.2fms [~8.2f%] ~9.2fms [~8.2f%]~n", [L, T1 + 0.0, 100.0, T2 + 0.0, 100.0]),
    compare(T, H),
    io:format("========================================================================~n").

%% 并行运行单项测试并计时
timer(Label, {sametime, F}, N) ->
    Watcher = spawn(?MODULE, watch, [self(), 0, N]),
    statistics(runtime),
    statistics(wall_clock),
    Fun = fun(I) -> 
        spawn(fun() -> F(I), Watcher ! ok end)
    end,
    for(1, N, Fun),
    receive
        finish -> finish
    end,
    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    U1 = Time1 * 1000 / N,
    U2 = Time2 * 1000 / N,
    io:format("~p [total: ~p(~p)ms avg: ~.3f(~.3f)us]~n", [Label, Time1, Time2, U1, U2]),
    [Label, Time1, Time2];

%% 运行单项测试并计时
timer(Label, F, N) ->
    statistics(runtime),
    statistics(wall_clock),
    for(1, N, F),
    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    U1 = Time1 * 1000 / N,
    U2 = Time2 * 1000 / N,
    io:format("~p [total: ~p(~p)ms avg: ~.3f(~.3f)us]~n", [Label, Time1, Time2, U1, U2]),
    [Label, Time1, Time2].

watch(Runer, I, End) ->
    receive
        ok -> 
            NewI = I + 1,
            case End =< NewI of
                false -> watch(Runer, NewI, End);
                true -> Runer ! finish 
            end
    end.

%% 比较结果
compare([], _) ->
    ok;
compare([[L, T1, T2] | T], [_, TB1, TB2] = TB) ->
    io:format("~-20s = ~9.2fms [~8.2f%] ~9.2fms [~8.2f%]~n", [L, T1 + 0.0, T1 / (TB1 + 0.00000001) * 100, T2 + 0.0, T2 / (TB2 + 0.00000001) * 100]),
    compare(T, TB).

%% 运行计时器(并发测试时使用)
ptimer(Label, N, F)->
    T = get_us(),
    for(1, N, F),
    DT = (get_us() - T),
    U = DT * 1000 / N,
    io:format("~p [total: ~.2fms avg: ~.2fus]~n", [Label, DT + 0.0, U + 0.0]),
    [Label, DT].

%% for循环
for(Max, Max, F) -> F(Max);
for(I, Max, F)   -> F(I), for(I + 1, Max, F).

%% 产生随机数
rand(Min, Max)->
    case get("rand_seed") of
        undefined ->
            RandSeed = erlang:now(),
            random:seed(RandSeed),
            put("rand_seed", RandSeed);
        _ ->
            skip
    end,
    M = Min - 1,
	random:uniform(Max - M) + M.

%% 取当前时间戳，单位:毫秒
get_us()->
    {M, S, U} = erlang:now(),
    M * 1000000000 + S * 1000 + U / 1000.

%% 暂停执行T毫秒
sleep(T) ->
    receive
    after T ->
            true
    end.
