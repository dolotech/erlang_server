-module(pt_unpack_client).
-export([p/2]).

p(10001, << Vtype:8,Vcode:32 >>) ->
    {ok, [Vtype,Vcode]};

p(_, <<>>) ->
    {ok, []};

p(10005, << VnoticeAL_:16,Vnotice/binary >>) ->
    VnoticeF_ = fun({B_, R_}) ->
            << Vid1:32,Vmsg1L_:16,Vmsg1:Vmsg1L_/binary,RB_/binary >> = B_, 
            {RB_, [[Vid1,Vmsg1]|R_]}
    end,
    {_, VnoticeR_} = protocol_fun:for(VnoticeAL_, VnoticeF_, {Vnotice, []}),
    {ok, [VnoticeR_]};

p(_, <<>>) ->
    {ok, []};

p(11000, << Vstatus:8,Vprogress:8,Vid:32 >>) ->
    {ok, [Vstatus,Vprogress,Vid]};

p(_, <<>>) ->
    {ok, []};

p(11001, << Vstate:8,Vprogress:8,Vid:32 >>) ->
    {ok, [Vstate,Vprogress,Vid]};

p(_, <<>>) ->
    {ok, []};

p(11002, << Vstate:8 >>) ->
    {ok, [Vstate]};

p(_, <<>>) ->
    {ok, []};

p(11003, << Vdiamond:32,Vcoin:32,Vtollgateid:32,Vtired:32,Vbagequ:16,Vbagprop:16,Vbagmat:16,VarenanameL_:16,Varenaname:VarenanameL_/binary,Vpicture:8 >>) ->
    {ok, [Vdiamond,Vcoin,Vtollgateid,Vtired,Vbagequ,Vbagprop,Vbagmat,Varenaname,Vpicture]};

p(_, <<>>) ->
    {ok, []};

p(11004, << Vhonor:32 >>) ->
    {ok, [Vhonor]};

p(_, <<>>) ->
    {ok, []};

p(11005, << Vcoin:32 >>) ->
    {ok, [Vcoin]};

p(_, <<>>) ->
    {ok, []};

p(11006, << Vdiamond:32 >>) ->
    {ok, [Vdiamond]};

p(_, <<>>) ->
    {ok, []};

p(11007, << Vtired:32,Vtime:32 >>) ->
    {ok, [Vtired,Vtime]};

p(_, <<>>) ->
    {ok, []};

p(11008, << Vluck:32 >>) ->
    {ok, [Vluck]};

p(_, <<>>) ->
    {ok, []};

p(11101, << Vstate:8,Vprogress:8,Vid:32 >>) ->
    {ok, [Vstate,Vprogress,Vid]};

p(_, <<>>) ->
    {ok, []};

p(11102, << Vcode:8 >>) ->
    {ok, [Vcode]};

p(_, <<>>) ->
    {ok, []};

p(11103, << Vcode:8 >>) ->
    {ok, [Vcode]};

p(_, <<>>) ->
    {ok, []};

p(13001, Bin) ->
    protocol_fun:unpack(13001, [int8,[int32,int32,int32,int8,[int32,int32],int32,int32,int32,int32,int32,int32,int32,int32,int32,int32],[int32,int32,int8]], Bin);

p(_, <<>>) ->
    {ok, []};

p(13002, << Vcode:8 >>) ->
    {ok, [Vcode]};

p(_, <<>>) ->
    {ok, []};

p(13005, << Vcode:8,Vequid:32,Vlevel:8,Vtime:16 >>) ->
    {ok, [Vcode,Vequid,Vlevel,Vtime]};

p(_, <<>>) ->
    {ok, []};

p(13006, << Vcode:8,Vtime:16 >>) ->
    {ok, [Vcode,Vtime]};

p(_, <<>>) ->
    {ok, []};

p(13010, << Vcode:8,Vequid:32 >>) ->
    {ok, [Vcode,Vequid]};

p(_, <<>>) ->
    {ok, []};

p(13012, << Vcode:8 >>) ->
    {ok, [Vcode]};

p(_, <<>>) ->
    {ok, []};

p(13021, << Vcode:8 >>) ->
    {ok, [Vcode]};

p(_, <<>>) ->
    {ok, []};

p(13022, << Vcode:8,Vlevel:8,Vid:32,Vtype:32 >>) ->
    {ok, [Vcode,Vlevel,Vid,Vtype]};

p(_, <<>>) ->
    {ok, []};

p(13024, << VmagicOrbsAL_:16,VmagicOrbs/binary >>) ->
    VmagicOrbsF_ = fun({B_, R_}) ->
            << Vlevel1:8,Vstate1:8,RB_/binary >> = B_, 
            {RB_, [[Vlevel1,Vstate1]|R_]}
    end,
    {_, VmagicOrbsR_} = protocol_fun:for(VmagicOrbsAL_, VmagicOrbsF_, {VmagicOrbs, []}),
    {ok, [VmagicOrbsR_]};

p(_, <<>>) ->
    {ok, []};

p(13025, << Vcode:8 >>) ->
    {ok, [Vcode]};

p(_, <<>>) ->
    {ok, []};

p(13026, << Vstate:8,Vdays:8 >>) ->
    {ok, [Vstate,Vdays]};

p(_, <<>>) ->
    {ok, []};

p(13027, << Vcode:8,Vtab:8,Vbags:16 >>) ->
    {ok, [Vcode,Vtab,Vbags]};

p(_, <<>>) ->
    {ok, []};

p(13028, << Vcode:8 >>) ->
    {ok, [Vcode]};

p(_, <<>>) ->
    {ok, []};

p(13030, << VpropsAL_:16,Vprops/binary >>) ->
    VpropsF_ = fun({B_, R_}) ->
            << Vid1:32,Vpile1:8,RB_/binary >> = B_, 
            {RB_, [[Vid1,Vpile1]|R_]}
    end,
    {_, VpropsR_} = protocol_fun:for(VpropsAL_, VpropsF_, {Vprops, []}),
    {ok, [VpropsR_]};

p(_, <<>>) ->
    {ok, []};

p(13032, << Vcode:8 >>) ->
    {ok, [Vcode]};

p(_, <<>>) ->
    {ok, []};

p(13040, << Vcode:8,Vdiamond:32,Vpos:8 >>) ->
    {ok, [Vcode,Vdiamond,Vpos]};

p(_, <<>>) ->
    {ok, []};

p(13042, << VrecentAL_:16,Vrecent/binary >>) ->
    VrecentF_ = fun({B_, R_}) ->
            << Vname1L_:16,Vname1:Vname1L_/binary,Vreward_id1:32,Vreward_num1:32,RB_/binary >> = B_, 
            {RB_, [[Vname1,Vreward_id1,Vreward_num1]|R_]}
    end,
    {_, VrecentR_} = protocol_fun:for(VrecentAL_, VrecentF_, {Vrecent, []}),
    {ok, [VrecentR_]};

p(_, <<>>) ->
    {ok, []};

p(13044, << VrecentAL_:16,Vrecent/binary >>) ->
    VrecentF_ = fun({B_, R_}) ->
            << Vname1L_:16,Vname1:Vname1L_/binary,Vvalues1:32,Vsum1:32,RB_/binary >> = B_, 
            {RB_, [[Vname1,Vvalues1,Vsum1]|R_]}
    end,
    {_, VrecentR_} = protocol_fun:for(VrecentAL_, VrecentF_, {Vrecent, []}),
    {ok, [VrecentR_]};

p(_, <<>>) ->
    {ok, []};

p(13045, << Vid:8,Vluck:32,Vvalues:32 >>) ->
    {ok, [Vid,Vluck,Vvalues]};

p(_, <<>>) ->
    {ok, []};

p(14001, << Vstatus:8 >>) ->
    {ok, [Vstatus]};

p(_, <<>>) ->
    {ok, []};

p(14002, << VheroesAL_:16,Vheroes/binary >>) ->
    VheroesF_ = fun({B_, R_}) ->
            << Vid1:32,Vtype1:32,Vseat1:8,Vquality1:8,Vlevel1:8,Vexp1:32,Vhp1:32,Vattack1:32,Vdefend1:32,Vpuncture1:32,Vhit1:32,Vdodge1:32,Vcrit1:32,VcritPercentage1:32,VanitCrit1:32,Vtoughness1:32,RB_/binary >> = B_, 
            {RB_, [[Vid1,Vtype1,Vseat1,Vquality1,Vlevel1,Vexp1,Vhp1,Vattack1,Vdefend1,Vpuncture1,Vhit1,Vdodge1,Vcrit1,VcritPercentage1,VanitCrit1,Vtoughness1]|R_]}
    end,
    {_, VheroesR_} = protocol_fun:for(VheroesAL_, VheroesF_, {Vheroes, []}),
    {ok, [VheroesR_]};

p(_, <<>>) ->
    {ok, []};

p(14010, << Vcode:8 >>) ->
    {ok, [Vcode]};

p(_, <<>>) ->
    {ok, []};

p(14015, << VheroesAL_:16,Vheroes/binary >>) ->
    VheroesF_ = fun({B_, R_}) ->
            << Vid1:32,RB_/binary >> = B_, 
            {RB_, [[Vid1]|R_]}
    end,
    {_, VheroesR_} = protocol_fun:for(VheroesAL_, VheroesF_, {Vheroes, []}),
    {ok, [VheroesR_]};

p(_, <<>>) ->
    {ok, []};

p(14019, << Vcode:8 >>) ->
    {ok, [Vcode]};

p(_, <<>>) ->
    {ok, []};

p(14020, << Vcode:8,Vcd:32,VheroesAL_:16,Vheroes/binary >>) ->
    VheroesF_ = fun({B_, R_}) ->
            << Vid1:8,Vtype1:32,Vlock1:8,Vquality1:8,Vravity1:8,RB_/binary >> = B_, 
            {RB_, [[Vid1,Vtype1,Vlock1,Vquality1,Vravity1]|R_]}
    end,
    {_, VheroesR_} = protocol_fun:for(VheroesAL_, VheroesF_, {Vheroes, []}),
    {ok, [Vcode,Vcd,VheroesR_]};

p(_, <<>>) ->
    {ok, []};

p(14022, << Vcode:8 >>) ->
    {ok, [Vcode]};

p(_, <<>>) ->
    {ok, []};

p(14023, << Vcode:8 >>) ->
    {ok, [Vcode]};

p(_, <<>>) ->
    {ok, []};

p(14025, << Vid:32,Vtype:32,Vseat:8,Vquality:8,Vlevel:8,Vexp:32,Vhp:32,Vattack:32,Vdefend:32,Vpuncture:32,Vhit:32,Vdodge:32,Vcrit:32,VcritPercentage:32,VanitCrit:32,Vtoughness:32 >>) ->
    {ok, [Vid,Vtype,Vseat,Vquality,Vlevel,Vexp,Vhp,Vattack,Vdefend,Vpuncture,Vhit,Vdodge,Vcrit,VcritPercentage,VanitCrit,Vtoughness]};

p(_, <<>>) ->
    {ok, []};

p(14030, << Vcode:8,Vheroid:32,Vlevel:8,Vexp:32 >>) ->
    {ok, [Vcode,Vheroid,Vlevel,Vexp]};

p(_, <<>>) ->
    {ok, []};

p(22002, Bin) ->
    protocol_fun:unpack(22002, [int32,int16,int8,[int8,[int32],[int8,int32,int8,[int32]],int32],[int32,int8,int32]], Bin);

p(_, <<>>) ->
    {ok, []};

p(22010, << Vgate:16,Vnum:8,Vtime:32 >>) ->
    {ok, [Vgate,Vnum,Vtime]};

p(_, <<>>) ->
    {ok, []};

p(23001, << Vcode:8 >>) ->
    {ok, [Vcode]};

p(_, <<>>) ->
    {ok, []};

p(23004, << Vrank:32,Vpoint:32,Vhonor:32,Vlevel:8 >>) ->
    {ok, [Vrank,Vpoint,Vhonor,Vlevel]};

p(_, <<>>) ->
    {ok, []};

p(23012, << Vcode:8 >>) ->
    {ok, [Vcode]};

p(_, <<>>) ->
    {ok, []};

p(23014, << VlistsAL_:16,Vlists/binary >>) ->
    VlistsF_ = fun({B_, R_}) ->
            << Vid1:32,Vname1L_:16,Vname1:Vname1L_/binary,RB_/binary >> = B_, 
            {RB_, [[Vid1,Vname1]|R_]}
    end,
    {_, VlistsR_} = protocol_fun:for(VlistsAL_, VlistsF_, {Vlists, []}),
    {ok, [VlistsR_]};

p(_, <<>>) ->
    {ok, []};

p(23015, << Vnumber:8,Vcd:32,Vchance:8,Vteam1:8,Vteam2:8,VtargetsAL_:16,Vtargets/binary >>) ->
    VtargetsF_ = fun({B_, R_}) ->
            << Vid1:32,Vname1L_:16,Vname1:Vname1L_/binary,Vpicture1:8,Vbeat1:8,RB_/binary >> = B_, 
            {RB_, [[Vid1,Vname1,Vpicture1,Vbeat1]|R_]}
    end,
    {_, VtargetsR_} = protocol_fun:for(VtargetsAL_, VtargetsF_, {Vtargets, []}),
    {ok, [Vnumber,Vcd,Vchance,Vteam1,Vteam2,VtargetsR_]};

p(_, <<>>) ->
    {ok, []};

p(23016, << VlistsAL_:16,Vlists/binary >>) ->
    VlistsF_ = fun({B_, R_}) ->
            << Vindex1:16,Vid1:32,Vname1L_:16,Vname1:Vname1L_/binary,Vexp1:16,Vfighting1:32,RB_/binary >> = B_, 
            {RB_, [[Vindex1,Vid1,Vname1,Vexp1,Vfighting1]|R_]}
    end,
    {_, VlistsR_} = protocol_fun:for(VlistsAL_, VlistsF_, {Vlists, []}),
    {ok, [VlistsR_]};

p(_, <<>>) ->
    {ok, []};

p(23017, << Vcode:8 >>) ->
    {ok, [Vcode]};

p(_, <<>>) ->
    {ok, []};

p(23018, << VactiveAL_:16,Vactive/binary >>) ->
    VactiveF_ = fun({B_, R_}) ->
            << Vid1:32,Vname11L_:16,Vname11:Vname11L_/binary,Vname21L_:16,Vname21:Vname21L_/binary,RB_/binary >> = B_, 
            {RB_, [[Vid1,Vname11,Vname21]|R_]}
    end,
    {_, VactiveR_} = protocol_fun:for(VactiveAL_, VactiveF_, {Vactive, []}),
    {ok, [VactiveR_]};

p(_, <<>>) ->
    {ok, []};

p(23022, << Vcode:8,Vtime:32,Vteam1:8,Vteam2:8,VtargetsAL_:16,Vtargets/binary >>) ->
    VtargetsF_ = fun({B_, R_}) ->
            << Vid1:32,Vname1L_:16,Vname1:Vname1L_/binary,Vpicture1:8,Vbeat1:8,RB_/binary >> = B_, 
            {RB_, [[Vid1,Vname1,Vpicture1,Vbeat1]|R_]}
    end,
    {_, VtargetsR_} = protocol_fun:for(VtargetsAL_, VtargetsF_, {Vtargets, []}),
    {ok, [Vcode,Vtime,Vteam1,Vteam2,VtargetsR_]};

p(_, <<>>) ->
    {ok, []};

p(23024, << Vcode:8,Vnum:8 >>) ->
    {ok, [Vcode,Vnum]};

p(_, <<>>) ->
    {ok, []};

p(23025, << Vcode:8,VmessegeAL_:16,Vmessege/binary >>) ->
    VmessegeF_ = fun({B_, R_}) ->
            << Vtype1:32,Vlevel1:16,Vhp1:32,Vseat1:16,Vweapon1:8,RB_/binary >> = B_, 
            {RB_, [[Vtype1,Vlevel1,Vhp1,Vseat1,Vweapon1]|R_]}
    end,
    {_, VmessegeR_} = protocol_fun:for(VmessegeAL_, VmessegeF_, {Vmessege, []}),
    {ok, [Vcode,VmessegeR_]};

p(_, <<>>) ->
    {ok, []};

p(23028, << Vcode:8,Vgold:32,Vhonor:32 >>) ->
    {ok, [Vcode,Vgold,Vhonor]};

p(_, <<>>) ->
    {ok, []};

p(23029, << Vcode:8,Vgold:32,Vhonor:32,Vpoint:16 >>) ->
    {ok, [Vcode,Vgold,Vhonor,Vpoint]};

p(_, <<>>) ->
    {ok, []};

p(23033, << Vgold:32,Vadd_exp:32,Vlev:8,Vexp:32 >>) ->
    {ok, [Vgold,Vadd_exp,Vlev,Vexp]};

p(_, <<>>) ->
    {ok, []};

p(24001, << Vid:32 >>) ->
    {ok, [Vid]};

p(_, <<>>) ->
    {ok, []};

p(24004, << Vcode:8,Vid:16,Vtype:8 >>) ->
    {ok, [Vcode,Vid,Vtype]};

p(_, <<>>) ->
    {ok, []};

p(24006, << VidsAL_:16,Vids/binary >>) ->
    VidsF_ = fun({B_, R_}) ->
            << Vid1:32,Vtype1:8,RB_/binary >> = B_, 
            {RB_, [[Vid1,Vtype1]|R_]}
    end,
    {_, VidsR_} = protocol_fun:for(VidsAL_, VidsF_, {Vids, []}),
    {ok, [VidsR_]};

p(_, <<>>) ->
    {ok, []};

p(Cmd, Data) -> 
    io:format("undefined_pt_unpack_client_cmd:~w, data:~w", [Cmd, Data]),
    {error, undefined_pt_unpack_client_cmd}.

%% vim: ft=erlang :
