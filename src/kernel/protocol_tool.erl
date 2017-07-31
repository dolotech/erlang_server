%%---------------------------------------------------
%% Protocol generate tool
%% 本协议生成工具已停用，改为PHP同时生成前后端协议
%%
%% @author rolong@vip.qq.com
%%---------------------------------------------------

-module(protocol_tool).
-export([main/0, main/1]).

-include_lib("kernel/include/file.hrl").

main() -> main(["src/pt/"]).

main([]) -> main(["src/pt/"]);

main([Path]) ->
    %% io:format("~p", [Path]),
    pt_server(Path),
    pt_client(Path),
    ok.

pt_server(SrcDir) ->
    PackTxt = SrcDir ++ "pt_pack.txt",
    PackErl = SrcDir ++ "pt_pack.erl",
    PackMod = "pt_pack",
    UnPackTxt = SrcDir ++ "pt_unpack.txt",
    UnPackErl = SrcDir ++ "pt_unpack.erl",
    UnPackMod = "pt_unpack",
    case {filelib:is_file(PackErl), filelib:is_file(UnPackErl)} of
        {true, true} ->
            {ok, PackTxtInfo} = file:read_file_info(PackTxt),
            {ok, PackErlInfo} = file:read_file_info(PackErl),
            {Days1, _Times1} =  calendar:time_difference(PackTxtInfo#file_info.mtime, PackErlInfo#file_info.mtime),
            {ok, UnPackTxtInfo} = file:read_file_info(UnPackTxt),
            {ok, UnPackErlInfo} = file:read_file_info(UnPackErl),
            {Days2, _Times2} =  calendar:time_difference(UnPackTxtInfo#file_info.mtime, UnPackErlInfo#file_info.mtime),
            case Days1 < 0 of
                true -> gen_pack_code({PackTxt, PackErl, PackMod});
                false -> ok
            end,
            case Days2 < 0 of
                true -> gen_unpack_code({UnPackTxt, UnPackErl, UnPackMod});
                false -> ok
            end,
            ok;
        {false, false} ->
            gen_pack_code({PackTxt, PackErl, PackMod}),
            gen_unpack_code({UnPackTxt, UnPackErl, UnPackMod}),
            ok;
        {false, true} ->
            gen_pack_code({PackTxt, PackErl, PackMod}),
            ok;
        {true, false} ->
            gen_unpack_code({UnPackTxt, UnPackErl, UnPackMod}),
            ok
    end,
    ok.

pt_client(SrcDir) ->
    PackTxt = SrcDir ++ "pt_unpack.txt",
    PackErl = SrcDir ++ "pt_pack_client.erl",
    PackMod = "pt_pack_client",
    UnPackTxt = SrcDir ++ "pt_pack.txt",
    UnPackErl = SrcDir ++ "pt_unpack_client.erl",
    UnPackMod = "pt_unpack_client",
    case {filelib:is_file(PackErl), filelib:is_file(UnPackErl)} of
        {true, true} ->
            {ok, PackTxtInfo} = file:read_file_info(PackTxt),
            {ok, PackErlInfo} = file:read_file_info(PackErl),
            {Days1, _Times1} =  calendar:time_difference(PackTxtInfo#file_info.mtime, PackErlInfo#file_info.mtime),
            {ok, UnPackTxtInfo} = file:read_file_info(UnPackTxt),
            {ok, UnPackErlInfo} = file:read_file_info(UnPackErl),
            {Days2, _Times2} =  calendar:time_difference(UnPackTxtInfo#file_info.mtime, UnPackErlInfo#file_info.mtime),
            case Days1 < 0 of
                true -> gen_client_pack_code({PackTxt, PackErl, PackMod});
                false -> ok
            end,
            case Days2 < 0 of
                true -> gen_unpack_code({UnPackTxt, UnPackErl, UnPackMod});
                false -> ok
            end,
            ok;
        {false, false} ->
            gen_client_pack_code({PackTxt, PackErl, PackMod}),
            gen_unpack_code({UnPackTxt, UnPackErl, UnPackMod}),
            ok;
        {false, true} ->
            gen_client_pack_code({PackTxt, PackErl, PackMod}),
            ok;
        {true, false} ->
            gen_unpack_code({UnPackTxt, UnPackErl, UnPackMod}),
            ok
    end,
    ok.

gen_pack_code({RulePath, TargetPath, Mod}) ->
    {ok, CmdList} = file:consult(RulePath),
    PackCode = gen_pack_code(CmdList, ""),
    Code = "-module("++Mod++").
-export([p/2]).

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

" ++ PackCode ++ "p(Cmd, Data) -> 
    io:format(\"undefined_pack_cmd:~w, data:~w\", [Cmd, Data]),
    {error, undefined_pack_cmd}.
",
    io:format("Generate ~s ok!~n", [TargetPath]),
    file:write_file(TargetPath, Code, []).

gen_pack_code([{Cmd, Rule} | T], Result) ->
    [L, B, C] = pack_data(1, Rule, "", "", ""),
    CmdL = integer_to_list(Cmd),
    Code = "p("++CmdL++", ["++L++"]) ->"++C++"
    Data = <<"++B++">>,
    Len = byte_size(Data),
    {ok, <<Len:16, "++CmdL++":16, Data/binary>>};

",
    gen_pack_code(T, Result ++ Code);
gen_pack_code([], Result) -> Result.

gen_client_pack_code({RulePath, TargetPath, Mod}) ->
    {ok, CmdList} = file:consult(RulePath),
    PackCode = gen_client_pack_code(CmdList, ""),
    Code = "-module("++Mod++").
-export([p/3]).

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

" ++ PackCode ++ "p(Cmd, Data, _) -> 
    io:format(\"undefined_test_pack_cmd:~w, data:~w\", [Cmd, Data]),
    {error, undefined_test_pack_cmd}.
",
    io:format("Generate ~s ok!~n", [TargetPath]),
    file:write_file(TargetPath, Code, []).

gen_client_pack_code([{Cmd, Rule} | T], Result) ->
    [L, B, C] = pack_data(1, Rule, "", "", ""),
    CmdL = integer_to_list(Cmd),
    Code = "p("++CmdL++", ["++L++"], Index) ->"++C++"
    Data = <<"++B++">>,
    Len = byte_size(Data),
    {ok, <<Len:16, Index:8, "++CmdL++":16, Data/binary>>};

",
    gen_client_pack_code(T, Result ++ Code);
gen_client_pack_code([], Result) -> Result.
    

pack_data(Index, [int8 | Format], ResultL, ResultB, ResultC) ->
    Name = "X" ++ integer_to_list(Index),
    ResultL1 = case ResultL of
        "" -> Name;
        _ -> ResultL ++ "," ++ Name
    end,
    ResultB1 = case ResultB of
        "" -> Name ++ ":8";
        _ -> ResultB ++ "," ++ Name ++ ":8"
    end,
    pack_data(Index + 1, Format, ResultL1, ResultB1, ResultC);

pack_data(Index, [int16 | Format], ResultL, ResultB, ResultC) ->
    Name = "X" ++ integer_to_list(Index),
    ResultL1 = case ResultL of
        "" -> Name;
        _ -> ResultL ++ "," ++ Name
    end,
    ResultB1 = case ResultB of
        "" -> Name ++ ":16";
        _ -> ResultB ++ "," ++ Name ++ ":16"
    end,
    pack_data(Index + 1, Format, ResultL1, ResultB1, ResultC);

pack_data(Index, [int32 | Format], ResultL, ResultB, ResultC) ->
    Name = "X" ++ integer_to_list(Index),
    ResultL1 = case ResultL of
        "" -> Name;
        _ -> ResultL ++ "," ++ Name
    end,
    ResultB1 = case ResultB of
        "" -> Name ++ ":32";
        _ -> ResultB ++ "," ++ Name ++ ":32"
    end,
    pack_data(Index + 1, Format, ResultL1, ResultB1, ResultC);

pack_data(Index, [float2 | Format], ResultL, ResultB, ResultC) ->
    Name = "X" ++ integer_to_list(Index),
    ResultL1 = case ResultL of
        "" -> Name;
        _ -> ResultL ++ "," ++ Name
    end,
    Name1 = "(round(" ++ Name ++ "*100))",
    ResultB1 = case ResultB of
        "" -> Name1 ++ ":32";
        _ -> ResultB ++ "," ++ Name1 ++ ":32"
    end,
    pack_data(Index + 1, Format, ResultL1, ResultB1, ResultC);

pack_data(Index, [string | Format], ResultL, ResultB, ResultC) ->
    I = integer_to_list(Index),
    Name = "X" ++ I,
    LenC = "
    LenX" ++ I ++ " = byte_size(X" ++ I ++ "),",
    ResultL1 = case ResultL of
        "" -> Name;
        _ -> ResultL ++ "," ++ Name
    end,
    ResultB0 = case ResultB of "" -> ResultB; _ -> ResultB ++ "," end,
    ResultB1 = ResultB0 ++ "LenX" ++ I ++ ":16," ++ Name ++ "/binary",
    ResultC1 = ResultC ++ LenC,
    pack_data(Index + 1, Format, ResultL1, ResultB1, ResultC1);

%% Array
pack_data(Index, [FormatH | Format], ResultL, ResultB, C) ->
    I = integer_to_list(Index),
    Name = "X" ++ I,
    L = case ResultL of "" -> ResultL; _ -> ResultL ++ "," end,
    B = case ResultB of "" -> ResultB; _ -> ResultB ++ "," end,
    [L1, B1, C1] = pack_data(Index+100, FormatH, "", "", ""),
    C0 = "
    LenArr"++I++" = length("++Name++"),
    F"++I++" = fun
        (["++L1++"]) -> "++C1++" <<"++B1++">>;
        ({"++L1++"}) -> "++C1++" <<"++B1++">> end,
    B"++ I ++" = list_to_binary([F"++I++"(X) || X <- "++Name++"]),",
    L2 = L ++ Name,
    B2 = B ++ "LenArr"++I++":16, B"++ I ++"/binary",
    C2 = C ++ C0,
    pack_data(Index + 1, Format, L2, B2, C2);

pack_data(_, [], ResultL, ResultB, ResultC) ->
    [ResultL, ResultB, ResultC].

gen_unpack_code({RulePath, TargetPath, Mod}) ->
    {ok, CmdList} = file:consult(RulePath),
    PackCode = gen_unpack_code(CmdList, ""),
    Code = "-module("++Mod++").
-export([p/2]).

p(_, <<>>) ->
    {ok, []};

" ++ PackCode ++ "p(Cmd, Data) -> 
    io:format(\"undefined_"++Mod++"_cmd:~w, data:~w\", [Cmd, Data]),
    {error, undefined_unpack_cmd}.
",
    io:format("Generate ~s ok!~n", [TargetPath]),
    file:write_file(TargetPath, Code, []).

gen_unpack_code([{Cmd, Rule} | T], Result) ->
    RuleL = binary_to_list(term_to_string(Rule)),
    CmdL = integer_to_list(Cmd),
    Code = case string:str(RuleL, "],") > 0 orelse string:str(RuleL, "]]]") > 0 of
        true ->
            "p("++CmdL++", Bin) ->
    pt_fun:unpack("++CmdL++", "++RuleL++", Bin);

";
        false ->
            [L, B, C] = unpack_data(1, Rule, "", "", ""),
            "p("++CmdL++", <<"++B++">>) ->"++C++"
    {ok, [" ++ L ++ "]};

"
    end,
    gen_unpack_code(T, Result ++ Code);
gen_unpack_code([], Result) -> Result.
    

unpack_data(Index, [int8 | Format], ResultL, ResultB, ResultC) ->
    Name = "X" ++ integer_to_list(Index),
    ResultL1 = case ResultL of
        "" -> Name;
        _ -> ResultL ++ "," ++ Name
    end,
    ResultB1 = case ResultB of
        "" -> Name ++ ":8";
        _ -> ResultB ++ "," ++ Name ++ ":8"
    end,
    unpack_data(Index + 1, Format, ResultL1, ResultB1, ResultC);

unpack_data(Index, [int16 | Format], ResultL, ResultB, ResultC) ->
    Name = "X" ++ integer_to_list(Index),
    ResultL1 = case ResultL of
        "" -> Name;
        _ -> ResultL ++ "," ++ Name
    end,
    ResultB1 = case ResultB of
        "" -> Name ++ ":16";
        _ -> ResultB ++ "," ++ Name ++ ":16"
    end,
    unpack_data(Index + 1, Format, ResultL1, ResultB1, ResultC);

unpack_data(Index, [int32 | Format], ResultL, ResultB, ResultC) ->
    Name = "X" ++ integer_to_list(Index),
    ResultL1 = case ResultL of
        "" -> Name;
        _ -> ResultL ++ "," ++ Name
    end,
    ResultB1 = case ResultB of
        "" -> Name ++ ":32";
        _ -> ResultB ++ "," ++ Name ++ ":32"
    end,
    unpack_data(Index + 1, Format, ResultL1, ResultB1, ResultC);

unpack_data(Index, [float2 | Format], ResultL, ResultB, ResultC) ->
    Name = "X" ++ integer_to_list(Index),
    ResultL1 = case ResultL of
        "" -> Name ++ "/100";
        _ -> ResultL ++ "," ++ Name ++ "/100"
    end,
    ResultB1 = case ResultB of
        "" -> Name ++ ":32";
        _ -> ResultB ++ "," ++ Name ++ ":32"
    end,
    unpack_data(Index + 1, Format, ResultL1, ResultB1, ResultC);

unpack_data(Index, [string | Format], ResultL, ResultB, ResultC) ->
    I = integer_to_list(Index),
    Name = "X" ++ I,
    ResultL0 = case ResultL of "" -> ResultL; _ -> ResultL ++ "," end,
    ResultB0 = case ResultB of "" -> ResultB; _ -> ResultB ++ "," end,
    ResultL1 = ResultL0 ++ Name,
    ResultB1 = ResultB0 ++ "LenX" ++ I ++ ":16," ++ Name ++ ":LenX" ++ I ++ "/binary",
    unpack_data(Index + 1, Format, ResultL1, ResultB1, ResultC);

%% Array
unpack_data(Index, [FormatH | Format], ResultL, ResultB, C) ->
    I = integer_to_list(Index),
    Name = "X" ++ I,
    L = case ResultL of "" -> ResultL; _ -> ResultL ++ "," end,
    B = case ResultB of "" -> ResultB; _ -> ResultB ++ "," end,
    [L1, B1, C1] = unpack_data(Index+100, FormatH, "", "", ""),
    C0 = "
    F"++I++" = fun({B"++I++", Result"++I++"}) ->
            <<"++B1++",RemBin/binary>> = B"++I++", "++C1++"
            {RemBin, [["++L1++"]|Result"++I++"]}
    end,
    {_, L"++I++"} = pt_fun:for(LenArr"++I++", F"++I++", {"++Name++", []}),",
    L2 = L ++ "L" ++ I,
    B2 = B ++ "LenArr"++I++":16, "++ Name ++ "/binary",
    C2 = C ++ C0,
    unpack_data(Index + 1, Format, L2, B2, C2);

unpack_data(_, [], ResultL, ResultB, ResultC) ->
    [ResultL, ResultB, ResultC].

term_to_string(Term) ->
    erlang:list_to_bitstring(io_lib:format("~w", [Term])).
