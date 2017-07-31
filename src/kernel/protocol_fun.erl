%%----------------------------------------------------
%% 协议动态解包、封包函数
%%
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------


-module(protocol_fun).
-export([
        for/3
        ,unpack/2
        ,unpack/3
        ,unpack1/2
        ,client_pack/4
        ,pack/2
        ,pack/3
    ]
).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

pack(Rule, List) ->
    {ok, pack1(Rule, List, [])}.

%% pack(Cmd, Rule, Data) -> {ok, Bin}
pack(Cmd, [], []) -> {ok, <<0:16, Cmd:16>>};
pack(Cmd, Rule, List) ->
    Data = pack1(Rule, List, []),
    Len = byte_size(Data),
    Bin = <<Len:16, Cmd:16, Data/binary>>,
    {ok, Bin}.

unpack(Rule, Bin) ->
    {Result, <<>>} = unpack1(Rule, Bin, []),
    {ok, Result}.

%% unpack(Cmd, Rule, Bin) -> {ok, list()}
unpack(_Cmd, Rule, Bin) ->
    {Result, <<>>} = unpack1(Rule, Bin, []),
    {ok, Result}.

unpack1(Rule, Bin) -> 
    unpack1(Rule, Bin, []).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%' for
for(0, _, State) -> State;
for(I, F, State)   -> 
    %% io:format("~n~w~n", [I]),
    for(I - 1, F, F(State)).
%%.

%%'
unpack1([int8 | Format], Bin, Result) ->
    <<Data:8, Bin1/binary>> = Bin,
    unpack1(Format, Bin1, [Data | Result]);
unpack1([int16 | Format], Bin, Result) ->
    <<Data:16, Bin1/binary>> = Bin,
    unpack1(Format, Bin1, [Data | Result]);
unpack1([int32 | Format], Bin, Result) ->
    <<Data:32, Bin1/binary>> = Bin,
    unpack1(Format, Bin1, [Data | Result]);
unpack1([string | Format], Bin, Result) ->
    <<Len:16, Data:Len/binary, Bin1/binary>> = Bin,
    unpack1(Format, Bin1, [Data | Result]);
unpack1([float2 | Format], Bin, Result) ->
    <<Data:32, Bin1/binary>> = Bin,
    unpack1(Format, Bin1, [(Data / 100) | Result]);
unpack1([FormatH | Format], Bin, Result) ->
    <<Len:16, Bin1/binary>> = Bin,
    {Result1, Bin2} = unpack_array(FormatH, Bin1, Len, []),
    Result2 = lists:reverse(Result1),
    unpack1(Format, Bin2, [Result2 | Result]);
unpack1([], Bin, Result) -> 
    {lists:reverse(Result), Bin}.

unpack_array(_, Bin, 0, Result) -> {Result, Bin};
unpack_array(Format, Bin, Len, Result) ->
    {Result1, Bin1} = unpack1(Format, Bin, []),
    unpack_array(Format, Bin1, Len - 1, [Result1 | Result]).
%%.

%% client_pack(Cmd, Rule, Data, index) -> {ok, Bin}
client_pack(Cmd, [], [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};
client_pack(Cmd, Rule, List, Index) ->
    %% T = erlang:now(),
    Data = pack1(Rule, List, []),
    Len = byte_size(Data),
    Bin = <<Len:16, Index:8, Cmd:16, Data/binary>>,
    %% DT = timer:now_diff(erlang:now(), T) / 1000,
    %% case DT > 1 of
    %%     true ->
    %%         io:fomat("client_pack CMD:~w, PackTime:~w~n", [Cmd, DT]),
    %%         ok;
    %%     false -> skip
    %% end,
    {ok, Bin}.

%%'
pack1([int8 | Format], [Val | Data], Result) ->
    NewResult = [<<Val:8>> | Result],
    pack1(Format, Data, NewResult);
pack1([int16 | Format], [Val | Data], Result) ->
    NewResult = [<<Val:16>> | Result],
    pack1(Format, Data, NewResult);
pack1([int32 | Format], [Val | Data], Result) ->
    NewResult = [<<Val:32>> | Result],
    pack1(Format, Data, NewResult);
pack1([string | Format], [Val | Data], Result) ->
    Size = byte_size(Val), 
    NewResult = [<<Size:16, Val/binary>> | Result],
    pack1(Format, Data, NewResult);
pack1([FormatH | Format], [DataH | Data], Result) ->
    {ArrayResult, Len} = pack_array(FormatH, DataH, [], 0),
    Result1 = [<<Len:16>> | Result],
    Result2 = [ArrayResult | Result1],
    pack1(Format, Data, Result2);
pack1([], [], Result) ->
    Result1 = lists:reverse(Result),
    list_to_binary(Result1).

pack_array(Format, [Val1 | Data], Result, Index) ->
    Val = case is_tuple(Val1) of
        true -> tuple_to_list(Val1);
        false -> Val1
    end,
    NewVal = pack1(Format, Val, []),
    pack_array(Format, Data, [NewVal | Result], Index + 1);
pack_array(_, [], Result, Index) ->
    {Result, Index}.
%%.

%%% vim: set foldmethod=marker foldmarker=%%',%%.:
