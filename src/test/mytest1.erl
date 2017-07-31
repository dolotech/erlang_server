%%----------------------------------------------------
%% Test
%%
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(mytest1).
-compile(export_all).
-include("common.hrl").

%% bin_test() ->
%%     Data = [{name, xjl}, {phone, 88888888}],
%%     % Bin = term_to_binary([{name, rolong},{phone, 888888}]),
%%     Bin = term_to_binary(Data),
%%     db:execute(<<"insert test(bin) values(~s);">>, [Bin]).

%% tt2(Tid) ->
%%     Items = [
%%          #item{num = 118, tid = 1}
%%         ,#item{num = 116, tid = 2}
%%         ,#item{num = 118, tid = 3}
%%         ,#item{num = 114, tid = 3}
%%         ,#item{num = 115, tid = 5}
%%         ,#item{num = 112, tid = 5}
%%         ,#item{num = 117, tid = 8}
%%         ,#item{num = 118, tid = 6}
%%         ,#item{num = 112, tid = 9}
%%         ,#item{num = 111, tid = 9}
%%         ,#item{num = 118, tid = 9}
%%     ],
%%     Items1 = get_min_item(Tid, Items),
%%     ?INFO("~p", [Items]),
%%     ?INFO("~p", [Items1]).
%% 
%% get_min_item(Tid, Items) ->
%%     Items1 = [I || I = #item{tid = Tid1} <- Items, Tid1 == Tid],
%%     case sort_tid(Items1) of
%%         [] -> false;
%%         [I | _] -> I
%%     end.
%% 
%% sort_tid([]) -> [];
%% sort_tid([RR | T]) ->
%%     sort_tid([R || R <- T, R#item.tid < RR#item.tid])
%%      ++[RR] ++
%%     sort_tid([R || R <- T, R#item.tid >= RR#item.tid]).
%% 
%% %%' tt1
%% %% tt1(N) ->
%% %%     Q = <<97,99,99,111,117,110,116,95,105,100,61,114,111,98,111,116,39,115,34,83,57,57,49,49,38,115,101,114,118,101,114,105,100,61,49,38,116,105,109,101,61,49,51,55,52,49,49,56,53,53,56,38,115,105,103,110,97,116,117,114,101,61,100,51,49,49,97,101,49,48,52,97,49,49,50,97,100,98,97,100,51,102,102,100,101,48,57,100,102,51,52,98,49,97>>,
%% %%     Q1 = parse_qs(Q),
%% %%     Q2 = parse_qs3(Q),
%% %%     ?INFO("~s", [Q]),
%% %%     ?INFO("~w", [Q1]),
%% %%     ?INFO("~w", [Q2]),
%% %%     %% {K, V} = lists:keyfind(<<"account_id">>, 1, Q2),
%% %%     %% ?INFO("~s, ~s", [K, V]),
%% %%     F1 = fun(_I) ->
%% %%             parse_qs(Q)
%% %%     end,
%% %%     F2 = fun(_I) ->
%% %%             parse_qs1(Q)
%% %%     end,
%% %%     F3 = fun(_I) ->
%% %%             parse_qs3(Q)
%% %%     end,
%% %%     F4 = fun(_I) ->
%% %%             {K, V} = lists:keyfind(<<"account_id">>, 1, Q2)
%% %%     end,
%% %%     F5 = fun(_I) ->
%% %%             {K, V} = lists:keyfind("account_id", 1, Q1)
%% %%     end,
%% %%     tester:run(N, [
%% %%             {"Bin", F4}
%% %%             ,{"List", F5}
%% %%         ]
%% %%     ).
%% 
%% parse_qs3(B) ->
%%     BL = binary:split(B, <<38>>, [global]),
%%     [list_to_tuple(binary:split(B1, <<61>>)) || B1 <- BL].
%% 
%% parse_qs1(B) ->
%%     BL = binary:split(B, <<"&">>, [global]),
%%     [list_to_tuple(binary:split(B1, <<"=">>)) || B1 <- BL].
%% 
%% parse_qs2(B) ->
%%     BL = binary:split(B, <<"&">>, [global]),
%%     [list_to_tuple(binary:split(B1, <<"=">>)) || B1 <- BL].
%% 
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
%% %%.
%% 
%% 
%% 
%% %%' 读代码与读进程字典比较
%% %% t:t1(1000000).
%% %% "Read File" [total: 93(94)ms avg: 0.093(0.094)us]
%% %% "Read Dict" [total: 3838(3837)ms avg: 3.838(3.837)us]
%% %% ========================================================================
%% %% Read File            =     93.00ms [  100.00%]     94.00ms [  100.00%]
%% %% Read Dict            =   3838.00ms [ 4126.88%]   3837.00ms [ 4081.91%]
%% %% ========================================================================
%% t1(N)->
%%     F1 = fun(_I) ->
%%             data_lev:get(20)
%%     end,
%%     F2 = fun(_I)->
%%             srv_cache:get({lev, 20})
%%     end,
%%     tester:run(N, [
%%             {"Read File", F1}
%%             ,{"Read Dict", F2}
%%         ]
%%     ).
%% %%.
%% 
%% %%' os_env
%% os_env() ->
%%     ReOpts = [{return, list}, {parts, 2}, unicode],
%%     Os = [list_to_tuple(re:split(S, "=", ReOpts)) || S <- os:getenv()],
%%     %% Drop variables without a name (win32)
%%     [T1 || {K, _V} = T1 <- Os, K =/= []].
%% %%.
%% 
%% %%' pack test
%% %% (myserver1@127.0.0.1)16> mytest1:pack_test(100000).
%% %% "protocol pack" [total: 2250(2260)ms avg: 22.500(22.600)us]
%% %% "recursion pack" [total: 3840(3946)ms avg: 38.400(39.460)us]
%% %% ========================================================================
%% %% protocol pack        =   2250.00ms [  100.00%]   2260.00ms [  100.00%]
%% %% recursion pack       =   3840.00ms [  170.67%]   3946.00ms [  174.60%]
%% %% ========================================================================
%% pack(22002, [X1,X2,X3,X4,X5]) ->
%%     LenArr1 = length(X1),
%%     F1 = fun
%%         ([X101,X102,X103,X104,X105,X106]) -> 
%%             LenArr106 = length(X106),
%%             F106 = fun
%%                 ([X206]) ->  <<X206:32>>;
%%                 ({X206}) ->  <<X206:32>> end,
%%             B106 = list_to_binary([F106(X) || X <- X106]), <<X101:32,X102:32,X103:32,X104:8,X105:32,LenArr106:16, B106/binary>>;
%%         ({X101,X102,X103,X104,X105,X106}) -> 
%%             LenArr106 = length(X106),
%%             F106 = fun
%%                 ([X206]) ->  <<X206:32>>;
%%                 ({X206}) ->  <<X206:32>> end,
%%             B106 = list_to_binary([F106(X) || X <- X106]), <<X101:32,X102:32,X103:32,X104:8,X105:32,LenArr106:16, B106/binary>> end,
%%     B1 = list_to_binary([F1(X) || X <- X1]),
%%     LenArr2 = length(X2),
%%     F2 = fun
%%         ([X102,X103,X104,X105,X106,X107]) -> 
%%             LenArr103 = length(X103),
%%             F103 = fun
%%                 ([X203,X204,X205]) ->  <<X203:8,X204:32,X205:8>>;
%%                 ({X203,X204,X205}) ->  <<X203:8,X204:32,X205:8>> end,
%%             B103 = list_to_binary([F103(X) || X <- X103]),
%%             LenArr107 = length(X107),
%%             F107 = fun
%%                 ([X207,X208]) ->  <<X207:32,X208:32>>;
%%                 ({X207,X208}) ->  <<X207:32,X208:32>> end,
%%             B107 = list_to_binary([F107(X) || X <- X107]), <<X102:8,LenArr103:16, B103/binary,X104:8,X105:8,X106:32,LenArr107:16, B107/binary>>;
%%         ({X102,X103,X104,X105,X106,X107}) -> 
%%             LenArr103 = length(X103),
%%             F103 = fun
%%                 ([X203,X204,X205]) ->  <<X203:8,X204:32,X205:8>>;
%%                 ({X203,X204,X205}) ->  <<X203:8,X204:32,X205:8>> end,
%%             B103 = list_to_binary([F103(X) || X <- X103]),
%%             LenArr107 = length(X107),
%%             F107 = fun
%%                 ([X207,X208]) ->  <<X207:32,X208:32>>;
%%                 ({X207,X208}) ->  <<X207:32,X208:32>> end,
%%             B107 = list_to_binary([F107(X) || X <- X107]), <<X102:8,LenArr103:16, B103/binary,X104:8,X105:8,X106:32,LenArr107:16, B107/binary>> end,
%%     B2 = list_to_binary([F2(X) || X <- X2]),
%%     LenArr3 = length(X3),
%%     F3 = fun
%%         ([X103,X104]) ->  <<X103:32,X104:32>>;
%%         ({X103,X104}) ->  <<X103:32,X104:32>> end,
%%     B3 = list_to_binary([F3(X) || X <- X3]),
%%     LenArr4 = length(X4),
%%     F4 = fun
%%         ([X104,X105,X106,X107]) ->  <<X104:32,X105:8,X106:32,X107:32>>;
%%         ({X104,X105,X106,X107}) ->  <<X104:32,X105:8,X106:32,X107:32>> end,
%%     B4 = list_to_binary([F4(X) || X <- X4]),
%%     Data = <<LenArr1:16, B1/binary,LenArr2:16, B2/binary,LenArr3:16, B3/binary,LenArr4:16, B4/binary,X5:8>>,
%%     Len = byte_size(Data),
%%     {ok, <<Len:16, 22002:16, Data/binary>>};
%% 
%% pack(22002000, [X1,X2,X3,X4,X5]) ->
%%     pt_fun:pack(22002, [[int32,int32,int32,int8,int32,[int32]],[int8,[int8,int32,int8],int8,int8,int32,[int32,int32]],[int32,int32],[int32,int8,int32,int32],int8], [X1,X2,X3,X4,X5]).
%% 
%% 
%% pack_test(N) ->
%%     Combat =  [[[1,5022,693,14,30001,[]],
%%             [2,3346,1188,13,30008,[]],
%%             [3,2364,1278,12,30003,[]],
%%             [4,3832,840,11,30033,[]],
%%             [0,11577,2316,25,30010,[]],
%%             [0,2060,515,23,30006,[]],
%%             [0,2060,515,21,30006,[]]],
%%         [[11,[[21,1220,0]],1,0,0,[]],
%%             [21,[[11,2802,0]],1,1,0,[]],
%%             [12,[[21,0,0]],1,1,0,[]],
%%             [23,[[13,2831,0]],1,0,0,[]],
%%             [13,[[23,0,0]],1,1,0,[]],
%%             [25,[[12,0,0]],1,1,0,[]],
%%             [14,[[25,10884,0]],1,0,0,[]],
%%             [25,[[11,0,0]],1,1,0,[]],
%%             [13,[[25,9696,0]],1,0,0,[]],
%%             [14,[[25,9696,2]],1,0,0,[]],
%%             [25,[[13,515,0]],1,0,0,[]],
%%             [13,[[25,8508,0]],1,0,0,[]],
%%             [14,[[25,7122,0]],1,1,0,[]],
%%             [25,[[13,0,0]],1,0,0,[]],
%%             [14,[[25,7122,2]],1,1,0,[]],
%%             [25,[[14,2706,0]],1,0,0,[]],
%%             [14,[[25,6429,0]],1,0,0,[]],
%%             [25,[[14,390,0]],1,0,0,[]],
%%             [14,[[25,5736,0]],1,0,0,[]],
%%             [25,[[14,0,0]],1,0,0,[]]],
%%         [],
%%         [],1],
%% 
%%     F1 = fun(_I) ->
%%             pack(22002, Combat)
%%     end,
%%     F2 = fun(_I)->
%%             pack(22002000, Combat)
%%     end,
%%     tester:run(N, [
%%             {"protocol pack", F1}
%%             ,{"recursion pack", F2}
%%         ]
%%     ).
%% %%.
%% 
%% %%' db_test
%% db_test(N) ->
%%     Rid = util:unixtime(),
%%     %% Combat =  [[[1,5022,693,14,30001,[]],
%%     %%         [2,3346,1188,13,30008,[]],
%%     %%         [3,2364,1278,12,30003,[]],
%%     %%         [4,3832,840,11,30033,[]],
%%     %%         [0,11577,2316,25,30010,[]],
%%     %%         [0,2060,515,23,30006,[]],
%%     %%         [0,2060,515,21,30006,[]]],
%%     %%     [[11,[[21,1220,0]],1,0,0,[]],
%%     %%         [21,[[11,2802,0]],1,1,0,[]],
%%     %%         [12,[[21,0,0]],1,1,0,[]],
%%     %%         [23,[[13,2831,0]],1,0,0,[]],
%%     %%         [13,[[23,0,0]],1,1,0,[]],
%%     %%         [25,[[12,0,0]],1,1,0,[]],
%%     %%         [14,[[25,10884,0]],1,0,0,[]],
%%     %%         [25,[[11,0,0]],1,1,0,[]],
%%     %%         [13,[[25,9696,0]],1,0,0,[]],
%%     %%         [14,[[25,9696,2]],1,0,0,[]],
%%     %%         [25,[[13,515,0]],1,0,0,[]],
%%     %%         [13,[[25,8508,0]],1,0,0,[]],
%%     %%         [14,[[25,7122,0]],1,1,0,[]],
%%     %%         [25,[[13,0,0]],1,0,0,[]],
%%     %%         [14,[[25,7122,2]],1,1,0,[]],
%%     %%         [25,[[14,2706,0]],1,0,0,[]],
%%     %%         [14,[[25,6429,0]],1,0,0,[]],
%%     %%         [25,[[14,390,0]],1,0,0,[]],
%%     %%         [14,[[25,5736,0]],1,0,0,[]],
%%     %%         [25,[[14,0,0]],1,0,0,[]]],
%%     %%     [],
%%     %%     [],1],
%%     Combat =  [[[1,5022,693,14,30001,[]],
%%             [2,3346,1188,13,30008,[]],
%%             [3,2364,1278,12,30003,[]],
%%             [4,3832,840,11,30033,[]],
%%             [0,11577,2316,25,30010,[]],
%%             [0,2060,515,23,30006,[]],
%%             [0,2060,515,21,30006,[]]],
%%         [[11,[[21,1220,0]],1,0,0,[]],
%%             [21,[[11,2802,0]],1,1,0,[]],
%%             [12,[[21,0,0]],1,1,0,[]],
%%             [23,[[13,2831,0]],1,0,0,[]],
%%             [13,[[23,0,0]],1,1,0,[]],
%%             [25,[[12,0,0]],1,1,0,[]],
%%             [14,[[25,10884,0]],1,0,0,[]],
%%             [25,[[11,0,0]],1,1,0,[]],
%%             [13,[[25,9696,0]],1,0,0,[]],
%%             [14,[[25,9696,2]],1,0,0,[]],
%%             [25,[[13,515,0]],1,0,0,[]],
%%             [13,[[25,8508,0]],1,0,0,[]],
%%             [14,[[25,7122,0]],1,1,0,[]],
%%             [25,[[13,0,0]],1,0,0,[]],
%%             [14,[[25,7122,2]],1,1,0,[]],
%%             [25,[[14,2706,0]],1,0,0,[]],
%%             [14,[[25,6429,0]],1,0,0,[]],
%%             [25,[[14,390,0]],1,0,0,[]],
%%             [14,[[25,5736,0]],1,0,0,[]],
%%             [25,[[14,0,0]],1,0,0,[]]],
%%         [],
%%         [],1],
%%     %% Rid = util:unixtime(),
%%     %% ?INFO("Bin:~w, Str~w", [byte_size(Bin), byte_size(Str)]),
%% 
%%     %% F1 = fun(I) ->
%%     %%         Str = util:term_to_string(Combat),
%%     %%         db:execute("insert test_str(role_id, item_id, val) values (~s, ~s, ~s)", [Rid, I, Str])
%%     %% end,
%%     %% F2 = fun(I) ->
%%     %%         {ok, Bin} = pack(22002, Combat),
%%     %%         db:execute("insert test_blob(role_id, item_id, val) values (~s, ~s, ~s)", [Rid, I, Bin])
%%     %% end,
%%     %% tester:run(N, [
%%     %%         {"insert bin", F2}
%%     %%         ,{"insert str", F1}
%%     %%     ]
%%     %% ).
%%     {ok, Bin} = pack(22002, Combat),
%%     B1 = format_sql("insert test_blob(role_id, item_id, val) values (~s,~s,~s);", [Rid, N, Bin]),
%%     B2 = list_to_binary([<<"insert test_blob(role_id, item_id, val) values (">>, integer_to_list(Rid), <<",">>, integer_to_list(N+1), <<",'">>, Bin, <<"');">>]),
%%     % db:execute(B1),
%%     % db:execute(B2),
%%     % io:format("~s~n", [list_to_binary([256])]),
%%     % io:format("~s~n", [<<"1">>]),
%%     io:format("~s~n", [B1]),
%%     io:format("~s~n", [B2]),
%%     F1 = fun(I) ->
%%           format_sql("insert test_blob(role_id, item_id, val) values (~s,~s,~s);", [Rid, I, Bin])
%%     end,
%%     F2 = fun(I) ->
%%             list_to_binary([<<"insert test_blob(role_id, item_id, val) values (">>, integer_to_list(Rid), <<",">>, integer_to_list(I), <<",'">>, Bin, <<"');">>])
%%     end,
%%     F3 = fun(I) ->
%%             list_to_binary(["insert test_blob(role_id, item_id, val) values (", integer_to_list(Rid), ",", integer_to_list(I), ",'", Bin, "');"])
%%     end,
%%     tester:run(N, [
%%             {"format_sql", F1}
%%             ,{"list_to_binary", F2}
%%             ,{"list_to_binary", F3}
%%         ]
%%     ),
%%     ok.
%% 
%% format_sql(Sql, Args) when is_list(Sql) ->
%%     S = re:replace(Sql, "\\?", "~s", [global, {return, list}]),
%%     L = [ mysql:encode(A) || A <- Args],
%%     list_to_bitstring(io_lib:format(S, L));
%% format_sql(Sql, Args) when is_bitstring(Sql) ->
%%     format_sql(bitstring_to_list(Sql), Args).
%% 
%% %%.
%% 
%% 
%% %%' simple_test
%% simple_test(N) ->
%%     Sql = <<"insert test_blob('role_id', item_id, val) values ( where rid = r'\"olo\"n'g">>,
%%     S = re:replace(Sql, "\\'", "\\\\'", [global, {return, binary}]),
%%     S1 = binary:replace(Sql, <<39>>, <<92,39>>, []),
%%     S2 = binary:replace(Sql, <<39>>, <<92,39>>, [global]),
%%     io:format("~s~n", [S]),
%%     io:format("~s~n", [S1]),
%%     io:format("~s~n", [S2]),
%%     F1 = fun(_I) ->
%%             re:replace(Sql, "\\'", "\\\\'", [global, {return, binary}])
%%     end,
%%     F2 = fun(_I) ->
%%             binary:replace(Sql, <<"'">>, <<"\\'">>, [])
%%     end,
%%     F3 = fun(_I) ->
%%             binary:replace(Sql, <<"'">>, <<"\\'">>, [global])
%%     end,
%%     tester:run(N, [
%%             {"re:replace", F1}
%%             ,{"binary:replace", F2}
%%             ,{"binary:replace", F3}
%%         ]
%%     ),
%%     ok.
%% %%.
%% 
%% 
%% vim: set filetype=erlang foldmarker=%%',%%. foldmethod=marker:
