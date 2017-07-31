-module(pt_unpack).
-export([p/2]).

p(11000, << Vtype:8,Vrand:8,Vsid:16,VaccountL_:16,Vaccount:VaccountL_/binary,VpasswordL_:16,Vpassword:VpasswordL_/binary,VsignatureL_:16,Vsignature:VsignatureL_/binary >>) ->
    {ok, [Vtype,Vrand,Vsid,Vaccount,Vpassword,Vsignature]};

p(_, <<>>) ->
    {ok, []};

p(11001, << VloginL_:16,Vlogin:VloginL_/binary >>) ->
    {ok, [Vlogin]};

p(_, <<>>) ->
    {ok, []};

p(11002, << VnameL_:16,Vname:VnameL_/binary,Vsex:8 >>) ->
    {ok, [Vname,Vsex]};

p(_, <<>>) ->
    {ok, []};

p(11101, << VaccountL_:16,Vaccount:VaccountL_/binary,VpasswordL_:16,Vpassword:VpasswordL_/binary >>) ->
    {ok, [Vaccount,Vpassword]};

p(_, <<>>) ->
    {ok, []};

p(11102, << VaccountL_:16,Vaccount:VaccountL_/binary,VpasswordL_:16,Vpassword:VpasswordL_/binary >>) ->
    {ok, [Vaccount,Vpassword]};

p(_, <<>>) ->
    {ok, []};

p(11103, << VaccountL_:16,Vaccount:VaccountL_/binary >>) ->
    {ok, [Vaccount]};

p(_, <<>>) ->
    {ok, []};

p(13001, << Vtype:8 >>) ->
    {ok, [Vtype]};

p(_, <<>>) ->
    {ok, []};

p(13002, << VequipID:32,VheroID:32,VequipOriginalID:32 >>) ->
    {ok, [VequipID,VheroID,VequipOriginalID]};

p(_, <<>>) ->
    {ok, []};

p(13005, << Vequid:32,VenId1:32,VenId2:32,VenId3:32,Vpay:8 >>) ->
    {ok, [Vequid,VenId1,VenId2,VenId3,Vpay]};

p(_, <<>>) ->
    {ok, []};

p(13006, << Vequid:32 >>) ->
    {ok, [Vequid]};

p(_, <<>>) ->
    {ok, []};

p(13010, << Vequid:32,Vgemid:32 >>) ->
    {ok, [Vequid,Vgemid]};

p(_, <<>>) ->
    {ok, []};

p(13012, << Vid:32 >>) ->
    {ok, [Vid]};

p(_, <<>>) ->
    {ok, []};

p(13021, << Vid:32 >>) ->
    {ok, [Vid]};

p(_, <<>>) ->
    {ok, []};

p(13022, << Vlevel:8 >>) ->
    {ok, [Vlevel]};

p(_, <<>>) ->
    {ok, []};

p(13027, << Vtype:8,Vline:8,Vtab:8 >>) ->
    {ok, [Vtype,Vline,Vtab]};

p(_, <<>>) ->
    {ok, []};

p(13028, << Vtab:8,VidsAL_:16,Vids/binary >>) ->
    VidsF_ = fun({B_, R_}) ->
            << V01:32,RB_/binary >> = B_, 
            {RB_, [[V01]|R_]}
    end,
    {_, VidsR_} = protocol_fun:for(VidsAL_, VidsF_, {Vids, []}),
    {ok, [Vtab,VidsR_]};

p(_, <<>>) ->
    {ok, []};

p(13032, << Vtab:8,VidsAL_:16,Vids/binary >>) ->
    VidsF_ = fun({B_, R_}) ->
            << Vid1:32,Vnum1:8,RB_/binary >> = B_, 
            {RB_, [[Vid1,Vnum1]|R_]}
    end,
    {_, VidsR_} = protocol_fun:for(VidsAL_, VidsF_, {Vids, []}),
    {ok, [Vtab,VidsR_]};

p(_, <<>>) ->
    {ok, []};

p(13044, << Vtype:8 >>) ->
    {ok, [Vtype]};

p(_, <<>>) ->
    {ok, []};

p(14001, << VheroesAL_:16,Vheroes/binary >>) ->
    VheroesF_ = fun({B_, R_}) ->
            << Vid1:32,Vposition1:8,RB_/binary >> = B_, 
            {RB_, [[Vid1,Vposition1]|R_]}
    end,
    {_, VheroesR_} = protocol_fun:for(VheroesAL_, VheroesF_, {Vheroes, []}),
    {ok, [VheroesR_]};

p(_, <<>>) ->
    {ok, []};

p(14010, << Vheroid:32 >>) ->
    {ok, [Vheroid]};

p(_, <<>>) ->
    {ok, []};

p(14019, << Vid:32,Vlock:8 >>) ->
    {ok, [Vid,Vlock]};

p(_, <<>>) ->
    {ok, []};

p(14020, << Vtype:8 >>) ->
    {ok, [Vtype]};

p(_, <<>>) ->
    {ok, []};

p(14022, << Vid:32 >>) ->
    {ok, [Vid]};

p(_, <<>>) ->
    {ok, []};

p(14023, << Vid:32 >>) ->
    {ok, [Vid]};

p(_, <<>>) ->
    {ok, []};

p(14030, << Vheroid:32,Vid1:32,Vid2:32,Vid3:32,Vid4:32 >>) ->
    {ok, [Vheroid,Vid1,Vid2,Vid3,Vid4]};

p(_, <<>>) ->
    {ok, []};

p(22002, << Vtype:8,VcurrentCheckPoint:16,Vpos:8 >>) ->
    {ok, [Vtype,VcurrentCheckPoint,Vpos]};

p(_, <<>>) ->
    {ok, []};

p(22010, << Vid:8,Vtype:8 >>) ->
    {ok, [Vid,Vtype]};

p(_, <<>>) ->
    {ok, []};

p(23001, << VnameL_:16,Vname:VnameL_/binary,Vpicture:8 >>) ->
    {ok, [Vname,Vpicture]};

p(_, <<>>) ->
    {ok, []};

p(23012, << Vid:32 >>) ->
    {ok, [Vid]};

p(_, <<>>) ->
    {ok, []};

p(23025, << Vid:32,Vtype:8 >>) ->
    {ok, [Vid,Vtype]};

p(_, <<>>) ->
    {ok, []};

p(23029, << Vtype:8 >>) ->
    {ok, [Vtype]};

p(_, <<>>) ->
    {ok, []};

p(24004, << Vid:32 >>) ->
    {ok, [Vid]};

p(_, <<>>) ->
    {ok, []};

p(Cmd, Data) -> 
    io:format("undefined_pt_unpack_cmd:~w, data:~w", [Cmd, Data]),
    {error, undefined_unpack_cmd}.

%% vim: ft=erlang :
