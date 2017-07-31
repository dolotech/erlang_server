-module(pt_pack_client).
-export([p/3]).

p(11000, [Type,Rand,Sid,Account,Password,Signature], Index_) ->
    AccountL_ = byte_size(Account),    PasswordL_ = byte_size(Password),    SignatureL_ = byte_size(Signature),    Data = << Type:8,Rand:8,Sid:16,AccountL_:16,Account:AccountL_/binary,PasswordL_:16,Password:PasswordL_/binary,SignatureL_:16,Signature:SignatureL_/binary >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 11000:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(11001, [Login], Index_) ->
    LoginL_ = byte_size(Login),    Data = << LoginL_:16,Login:LoginL_/binary >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 11001:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(11002, [Name,Sex], Index_) ->
    NameL_ = byte_size(Name),    Data = << NameL_:16,Name:NameL_/binary,Sex:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 11002:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(11101, [Account,Password], Index_) ->
    AccountL_ = byte_size(Account),    PasswordL_ = byte_size(Password),    Data = << AccountL_:16,Account:AccountL_/binary,PasswordL_:16,Password:PasswordL_/binary >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 11101:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(11102, [Account,Password], Index_) ->
    AccountL_ = byte_size(Account),    PasswordL_ = byte_size(Password),    Data = << AccountL_:16,Account:AccountL_/binary,PasswordL_:16,Password:PasswordL_/binary >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 11102:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(11103, [Account], Index_) ->
    AccountL_ = byte_size(Account),    Data = << AccountL_:16,Account:AccountL_/binary >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 11103:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(13001, [Type], Index_) ->
    Data = << Type:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 13001:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(13002, [EquipID,HeroID,EquipOriginalID], Index_) ->
    Data = << EquipID:32,HeroID:32,EquipOriginalID:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 13002:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(13005, [Equid,EnId1,EnId2,EnId3,Pay], Index_) ->
    Data = << Equid:32,EnId1:32,EnId2:32,EnId3:32,Pay:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 13005:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(13006, [Equid], Index_) ->
    Data = << Equid:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 13006:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(13010, [Equid,Gemid], Index_) ->
    Data = << Equid:32,Gemid:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 13010:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(13012, [Id], Index_) ->
    Data = << Id:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 13012:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(13021, [Id], Index_) ->
    Data = << Id:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 13021:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(13022, [Level], Index_) ->
    Data = << Level:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 13022:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(13027, [Type,Line,Tab], Index_) ->
    Data = << Type:8,Line:8,Tab:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 13027:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(13028, [Tab,Ids], Index_) ->
    IdsAL_ = length(Ids),
    IdsR_ = list_to_binary([<<X:32>>||X <- Ids]),
    Data = << Tab:8,IdsAL_:16,IdsR_/binary >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 13028:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(13032, [Tab,Ids], Index_) ->
    IdsAL_ = length(Ids),
    IdsF_ = fun
        ([Id1,Num1]) ->
            << Id1:32,Num1:8 >>;
        ({Id1,Num1}) ->
            << Id1:32,Num1:8 >>
    end,
    IdsR_ = list_to_binary([IdsF_(X)||X <- Ids]),
    Data = << Tab:8,IdsAL_:16,IdsR_/binary >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 13032:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(13044, [Type], Index_) ->
    Data = << Type:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 13044:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(14001, [Heroes], Index_) ->
    HeroesAL_ = length(Heroes),
    HeroesF_ = fun
        ([Id1,Position1]) ->
            << Id1:32,Position1:8 >>;
        ({Id1,Position1}) ->
            << Id1:32,Position1:8 >>
    end,
    HeroesR_ = list_to_binary([HeroesF_(X)||X <- Heroes]),
    Data = << HeroesAL_:16,HeroesR_/binary >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 14001:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(14010, [Heroid], Index_) ->
    Data = << Heroid:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 14010:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(14019, [Id,Lock], Index_) ->
    Data = << Id:32,Lock:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 14019:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(14020, [Type], Index_) ->
    Data = << Type:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 14020:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(14022, [Id], Index_) ->
    Data = << Id:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 14022:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(14023, [Id], Index_) ->
    Data = << Id:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 14023:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(14030, [Heroid,Id1,Id2,Id3,Id4], Index_) ->
    Data = << Heroid:32,Id1:32,Id2:32,Id3:32,Id4:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 14030:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(22002, [Type,CurrentCheckPoint,Pos], Index_) ->
    Data = << Type:8,CurrentCheckPoint:16,Pos:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 22002:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(22010, [Id,Type], Index_) ->
    Data = << Id:8,Type:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 22010:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(23001, [Name,Picture], Index_) ->
    NameL_ = byte_size(Name),    Data = << NameL_:16,Name:NameL_/binary,Picture:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 23001:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(23012, [Id], Index_) ->
    Data = << Id:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 23012:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(23025, [Id,Type], Index_) ->
    Data = << Id:32,Type:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 23025:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(23029, [Type], Index_) ->
    Data = << Type:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 23029:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(24004, [Id], Index_) ->
    Data = << Id:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, Index_:8, 24004:16, Data/binary >>};

p(Cmd, [], Index) -> {ok, <<0:16, Index:8, Cmd:16>>};

p(Cmd, Data, _Index) -> 
    io:format("undefined_pack_client_cmd:~w, data:~w", [Cmd, Data]),
    {error, undefined_pack_client_cmd}.

%% vim: ft=erlang :
