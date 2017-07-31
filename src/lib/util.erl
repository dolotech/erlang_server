%%----------------------------------------------------
%% 工具包
%%
%% $Id: util.erl 5772 2013-12-07 09:34:35Z rolong $
%% @author Rolong<rolong@vip.qq.com>
%%----------------------------------------------------

-module(util).
-export([
        parse_qs/1
        ,for/3
        ,for/4
        ,floor/1
        ,ceil/1
        ,unixtime/0
        ,unixtime/1
        ,sleep/1
        ,print/1
        ,rand/1
        ,rand/2
        ,rand_float1/2
        ,rand_float2/2
        ,rand_element/1
        ,rand_element/2
        ,rand_element1/1
        ,rand_element1/2
        ,find_repeat_element/1
        ,find_repeat_key_element/2
        ,store_element/2
        ,rate/1
        ,rate1000/1
        ,test/1
        ,test/0
        ,get_time_list/1
        ,get_day_list/1
        ,term_to_string/1
        ,bitstring_to_term/1
        ,string_to_term/1
        ,term_to_bitstring/1
        ,string_to_num/1
        ,info/1
        ,info/2
        ,info/4
        ,debug/1
        ,debug/2
        ,debug/4
        ,test_time_limit/2
        ,md5/1
        ,ip_to_binary/1
        ,test1/0
        ,test2/1
        ,print_self/0
        ,get_val/2
        ,get_val/3
        ,get_val1/2
        ,get_val1/3
        ,set_val/3
        ,in_time/2
        ,mktime/1
        ,get_bool_sign/2
        ,set_bool_sign/3
        ,print_bit/1
        ,fix_max/2
        ,get_range_data/1
        ,min0/1
        ,shuffle/1
        ,del_repeat_element/1
    ]
).
-include("common.hrl").

get_range_data(M) ->
    Rand = rand(M:get(range)),
    Ids = M:get(ids),
    case [{S, E} || {S, E} <- Ids, Rand >= S, Rand =< E] of
        [Key] -> M:get(Key);
        _ -> undefined
    end.

fix_max(Num, Max) when Num < Max -> Num;
fix_max(_Num, Max) -> Max.

min0(A) when A >= 0 -> A;
min0(_) -> 0.

%% 解析 QueryString
parse_qs(Bin) ->
    BL = binary:split(Bin, <<38>>, [global]),
    [list_to_tuple(binary:split(B1, <<61>>)) || B1 <- BL].

%% parse_qs(String) when is_bitstring(String) ->
%%     parse_qs(bitstring_to_list(String));
%% 
%% parse_qs(String) ->
%%     parse_qs(String, "&", "=").
%% 
%% parse_qs(String, Token1, Token2) when is_bitstring(String) ->
%%     parse_qs(bitstring_to_list(String), Token1, Token2);
%% 
%% parse_qs(String, Token1, Token2) ->
%%     [ list_to_tuple(string:tokens(KV, Token2)) || KV <- string:tokens(String, Token1) ].

%% for循环
for(Min, Max, _F) when Min>Max ->
    error;
for(Max, Max, F) ->
    F(Max);
for(I, Max, F)   ->
    F(I),
    for(I+1, Max, F).

%% 带返回状态的for循环
%% @return {ok, State}
for(Max, Min, _F, State) when Min<Max -> {ok, State};
for(Max, Max, F, State) -> F(Max, State);
for(I, Max, F, State)   -> 
    {ok, NewState} = F(I, State), 
    for(I+1, Max, F, NewState).

%% 取小于X的最大整数 
floor(X) ->
    T = erlang:trunc(X),
    case (X < T) of
        true -> T - 1;
        _ -> T
    end.

%% 取大于X的最小整数
ceil(X) ->
    T = erlang:trunc(X),
    case (X > T) of
        true -> T + 1;
        _ -> T
    end.

%% 取得当前的unix时间戳
unixtime() ->
    {M, S, _} = erlang:now(),
    M * 1000000 + S.

%% 取得当前unix时间戳，精确到毫秒3位
unixtime(micro) ->
    {M, S, Micro} = erlang:now(),
    M * 1000000 + S + Micro / 1000000;

%% 获取当天0时0分0秒的时间戳（这里是相对于当前时区而言，后面的unixtime调用都基于这个函数
unixtime(today) ->
    {M, S, MS} = now(),
    {_, Time} = calendar:now_to_local_time({M, S, MS}),
    M * 1000000 + S - calendar:time_to_seconds(Time);

%% 获取明天0时0分0秒的时间戳
unixtime(tomorrow) ->
    unixtime(today) + 86400;

%% 获取昨天0时0分0秒的时间戳
unixtime(yesterday) ->
    unixtime(today) - 86400;

%%  获取当天12时0分0秒的时间戳
unixtime(noon) ->
    unixtime(today) + 43200.

%% 暂停执行T毫秒
sleep(T) ->
    receive
    after T ->
            true
    end.

%% 测试用
print(Data) ->
    io:format("~ts~n", [xmerl_ucs:from_utf8(Data)]).

%% 产生一个Min到Max之间的随机整数
rand({Min, Max}) when Min > Max -> 
    rand(Max, Min);
rand({Min, Max}) -> rand(Min, Max);
rand([Min, Max]) when Min > Max -> 
    rand(Max, Min);
rand([Min, Max]) -> rand(Min, Max).

rand(Same, Same) -> Same;
rand(Min, Max) ->
    %% 如果没有种子，将从核心服务器中去获取一个种子，以保证不同进程都可取得不同的种子
    case get("rand_seed") of
        undefined ->
            RandSeed = myrandomseed:get_seed(),
            random:seed(RandSeed),
            put("rand_seed", RandSeed);
        _ -> skip
    end,
    M = Min - 1,
    random:uniform(Max - M) + M.

rand_float1(Min, Max) when is_float(Min); is_float(Max) ->
    rand(round(Min * 10), round(Max * 10)) / 10;
rand_float1(Min, Max) -> 
    rand(Min, Max).

rand_float2(Min, Max) when is_float(Min); is_float(Max) ->
    rand(round(Min * 100), round(Max * 100)) / 100;
rand_float2(Min, Max) -> 
    rand(Min, Max).

rand_element([]) -> undefined;
rand_element([I]) -> I;
rand_element(List) -> 
    Len = length(List),
    Nth = rand(1, Len),
    lists:nth(Nth, List).

rand_element(Num, List) ->
    rand_element(Num, List, []).

rand_element(0, _List, Reply) -> Reply;
rand_element(Num, List, Reply) ->
    case rand_element(List) of
        undefined -> Reply;
        E ->
            Reply1 = [E | Reply],
            List1 = lists:delete(E, List),
            rand_element(Num - 1, List1, Reply1)
    end.

rand_element1([]) -> undefined;
rand_element1([I]) -> I;
rand_element1(List) -> 
    Len = length(List),
    Nth = rand(1, Len),
    lists:nth(Nth, List).

rand_element1(Num, List) ->
    rand_element1(Num, List, []).

rand_element1(0, _List, Reply) -> Reply;
rand_element1(Num, List, Reply) ->
    case rand_element1(List) of
        undefined -> Reply;
        E ->
            case lists:member(E, Reply) of
                true -> rand_element1(Num, List, Reply);
                false ->
                    Reply1 = [E | Reply],
                    List1 = lists:delete(E, List),
                    rand_element1(Num - 1, List1, Reply1)
            end
    end.

find_repeat_element([]) -> [];
find_repeat_element(L) -> 
    find_repeat_element(L, []).

find_repeat_element([H | T], Reply) ->
    case lists:member(H, T) of
        true -> find_repeat_element(T, [H | Reply]);
        false -> find_repeat_element(T, Reply)
    end;
find_repeat_element([], Reply) -> Reply.

%%
find_repeat_key_element(_Nth, []) -> [];
find_repeat_key_element(Nth, L) -> 
    find_repeat_key_element(Nth, L, []).

find_repeat_key_element(Nth, [H | T], Reply) ->
    H1 = if
        is_tuple(H) -> tuple_to_list(H);
        true -> H
    end,
    Key = lists:nth(Nth, H1),
    case lists:keymember(Key, Nth, T) of
        true -> find_repeat_key_element(Nth, T, [H | Reply]);
        false -> 
            case lists:keymember(Key, Nth, Reply) of
                true -> find_repeat_key_element(Nth, T, [H | Reply]);
                false -> find_repeat_key_element(Nth, T, Reply)
            end
    end;
find_repeat_key_element(_Nth, [], Reply) -> Reply.

del_repeat_element([]) -> [];
del_repeat_element(L) -> 
    del_repeat_element(L, []).

del_repeat_element([H | T], Reply) ->
    case lists:member(H, Reply) of
        true -> del_repeat_element(T, Reply);
        false -> del_repeat_element(T, [H | Reply])
    end;
del_repeat_element([], Reply) -> Reply.

rate(Rate) ->
    R = Rate * 100,
    case rand(1, 10000) of
        N when N =< R -> true;
        _ -> false
    end.

%% 千分率
rate1000(Rate) ->
    R = Rate * 100,
    case rand(1, 100000) of
        N when N =< R -> true;
        _ -> false
    end.

test() ->
    ?INFO("test ...:~w", [test]),
    node().

test(T) ->
    ?INFO("test ...:~w", [T]),
    node().

get_day_list(now) ->
    {{Y,Mo,D}, _} = erlang:localtime(),
    [t(Y), t(Mo), t(D)].

get_time_list(now) ->
    {{Y,Mo,D}, {H,Mi,S}} = erlang:localtime(),
    [t(Y), t(Mo), t(D), t(H), t(Mi), t(S)].

%% 使用通用参数，转换list，int list [171,167,...] 不会保存成 string
term_to_string(Term) ->
    erlang:list_to_bitstring(io_lib:format("~w", [Term])).

%% term反序列化，bitstring转换为term，e.g., <<"[{a},1]">>  => [{a},1]
bitstring_to_term(undefined) -> undefined;
bitstring_to_term(BitString) ->
    string_to_term(binary_to_list(BitString)).

%% term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
string_to_term(String) ->
    case erl_scan:string(String++".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> undefined
            end;    
        _Error ->
            undefined
    end.

%% term序列化，term转换为bitstring格式，e.g., [{a},1] => <<"[{a},1]">> 
term_to_bitstring(Term) ->
    erlang:list_to_bitstring(io_lib:format("~p", [Term])).

%% 字符转数字，整数或者浮点数
string_to_num(String) ->
    case lists:member($., String) of
        true  -> list_to_float(String);
        false -> list_to_integer(String)
    end.

t(X) when is_integer(X) ->
    t1(integer_to_list(X));
t(_) -> "".

t1([X]) -> [$0,X];
t1(X)   -> X.

%% 普通信息
info(Msg) ->
    info(Msg, []).
info(Format, Args) ->
    info(Format, Args, null, null).
%% info(_Format, _Args, _Mod, _Line) -> ok.
info(Format, Args, Mod, Line) ->
    Msg = format("info", Format, Args, Mod, Line),
    io:format("~ts", [Msg]).

%% 调试信息
debug(Msg) ->
    debug(Msg, []).
debug(Format, Args) ->
    debug(Format, Args, null, null).
%%debug(_Format, _Args, _Mod, _Line) -> null.
debug(Format, Args, Mod, Line) ->
    case env:get(debug) of
        on ->
            Msg = format("debug", Format, Args, Mod, Line),
            io:format("~ts", [Msg]);
        _ -> ok
    end.

%% 格式化打印信息
%% T = "error" | "info" | "debug" 类型
%% F = list() 格式
%% A = list() 参数
%% Mod = list() 模块名
%% Line = int() 所在行
format(T, F, A, Mod, Line) ->
    {{Y, M, D}, {H, I, S}} = erlang:localtime(),
    Date = lists:concat([Y, "/", M, "/", D, " ", H, ":", I, ":", S]),
    case Line of
        null -> erlang:iolist_to_binary(io_lib:format(lists:concat(["# ", T, " ~s ", F, "~n"]), [Date] ++ A));
        _ -> erlang:iolist_to_binary(io_lib:format(lists:concat(["# ", T, " ~s ~w[~w:~w] ", F, "~n"]), [Date, self(), Mod, Line] ++ A))
    end.

%% 最小调用时间测试
test_time_limit(Key, MinTime) ->
    Now = erlang:now(),
    case get(Key) of
        undefined -> put(Key, Now);
        T-> 
            Td = timer:now_diff(Now, T) / 1000,
            case Td < MinTime of
                true ->
                    ?INFO("Invoked too fast! [Key:~w, Time:~wms]", [Key, Td]);
                false -> ok
            end,
            put(Key, Now)
    end.

test1() ->
    put('$test_time', erlang:now()).

test2(Msg) ->
    case get('$test_time') of
        undefined -> ?INFO("undefined '$test_time'");
        T1 ->
            Td = timer:now_diff(erlang:now(), T1) / 1000,
            ?INFO("[Run Time]~s: ~w ms", [Msg, Td])
    end.

%% 转换成HEX格式的md5
md5(S) ->
    list_to_binary([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

ip_to_binary(Ip) when is_binary(Ip) -> Ip;
ip_to_binary({Ip1, Ip2, Ip3, Ip4}) ->
    Ip0 = lists:concat([
            integer_to_list(Ip1), ".", 
            integer_to_list(Ip2), ".", 
            integer_to_list(Ip3), ".", 
            integer_to_list(Ip4)
        ]),
    list_to_binary(Ip0).

print_self() ->
    P = self(),
    Data = {
        P
        ,erlang:process_info(P, memory) 
        ,erlang:process_info(P, message_queue_len) 
    },
    io:format("~n~w", [Data]).

get_val(Key, L) ->
    get_val(Key, L, undefined).

get_val(Key, L, Default) when is_list(L) ->
    case lists:keyfind(Key, 1, L) of
        false -> Default;
        {_, V} -> V
    end;
get_val(_Key, _L, Default) -> Default.

get_val1(Key, L) ->
    get_val1(Key, L, undefined).

get_val1(Key, L, Default) when is_list(L) ->
    case lists:keyfind(Key, 1, L) of
        false -> Default;
        {_, [V1, V2]} -> rand(V1, V2);
        {_, V} -> V
    end;
get_val1(_Key, _L, Default) -> Default.

set_val(Key, Val, L) ->
    lists:keystore(Key, 1, L, {Key, Val}).

%% 返回：{{Y, M, D},{H, I, S}}
mktime({to_date, UnixSec})->
    DT = calendar:gregorian_seconds_to_datetime(UnixSec + calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})),
     erlang:universaltime_to_localtime(DT);

%% 生成一个指定日期的unix时间戳(无时区问题)
%% Date = date() = {Y, M, D}
%% Time = time() = {H, I, S}
%% 参数必须大于1970年1月1日
mktime({Date, Time}) ->
    DT = erlang:localtime_to_universaltime({Date, Time}),
    calendar:datetime_to_gregorian_seconds(DT) - calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}).

%% 判断现在是否在某年某月某日某时间段内
in_time({SYea, SMon, SDay, SH, SM, SS}, {EYea, EMon, EDay, EH, EM, ES}) -> 
    in_time(unixtime(), {SYea, SMon, SDay, SH, SM, SS}, {EYea, EMon, EDay, EH, EM, ES}).

%% 判断某时间是否在某年某月某日某时间段内
in_time(Timestamp, {SYea, SMon, SDay, SH, SM, SS}, {EYea, EMon, EDay, EH, EM, ES}) ->
    Timestamp >= mktime({{SYea, SMon, SDay}, {SH, SM, SS}}) andalso 
    Timestamp =< mktime({{EYea, EMon, EDay}, {EH, EM, ES}}).

print_bit(<<V1:1, V2:1, V3:1, V4:1, V5:1, V6:1, V7:1, V8:1, Rest/binary>>) ->
    ?INFO("~w", [{V1, V2, V3, V4, V5, V6, V7, V8}]),
    print_bit(Rest);
print_bit(<<>>) ->
    ok.

%% Pos:
%%     1=领取成长礼包
%% Sign band util:ceil(math:pow(32 - Pos)).
%% Sign bor util:ceil(math:pow(32 - Pos)).
get_bool_sign(Pos, Bin) ->
    BitLen = byte_size(Bin) * 8,
    Len1 = Pos - 1,
    Len2 = BitLen - Pos,
    <<_B1:Len1, B:1, _B2:Len2>> = Bin,
    B.

set_bool_sign(Pos, Val, Bin) when Val == 0; Val == 1 ->
    BitLen = byte_size(Bin) * 8,
    Len1 = Pos - 1,
    Len2 = BitLen - Pos,
    <<B1:Len1, _OldVal:1, B2:Len2>> = Bin,
    <<B1:Len1, Val:1, B2:Len2>>.

%% 打乱
shuffle(L) ->
    List1 = [{random:uniform(), X} || X <- L],
    List2 = lists:keysort(1, List1),
    [E || {_, E} <- List2].

store_element(E, L) ->
    case lists:member(E, L) of
        true -> L;
        false -> [E | L]
    end.

%%% vim: set foldmethod=marker foldmarker=%%',%%.:
