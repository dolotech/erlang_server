-module(pt_pack).
-export([p/2]).

p(10001, [Type,Code]) ->
    Data = << Type:8,Code:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 10001:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(10005, [Notice]) ->
    NoticeAL_ = length(Notice),
    NoticeF_ = fun
        ([Id1,Msg1]) ->
                Msg1L_ = byte_size(Msg1),<< Id1:32,Msg1L_:16,Msg1:Msg1L_/binary >>;
        ({Id1,Msg1}) ->
                Msg1L_ = byte_size(Msg1),<< Id1:32,Msg1L_:16,Msg1:Msg1L_/binary >>
    end,
    NoticeR_ = list_to_binary([NoticeF_(X)||X <- Notice]),
    Data = << NoticeAL_:16,NoticeR_/binary >>,
    Len = byte_size(Data),
    {ok, << Len:16, 10005:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(11000, [Status,Progress,Id]) ->
    Data = << Status:8,Progress:8,Id:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 11000:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(11001, [State,Progress,Id]) ->
    Data = << State:8,Progress:8,Id:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 11001:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(11002, [State]) ->
    Data = << State:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 11002:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(11003, [Diamond,Coin,Tollgateid,Tired,Bagequ,Bagprop,Bagmat,Arenaname,Picture]) ->
    ArenanameL_ = byte_size(Arenaname),    Data = << Diamond:32,Coin:32,Tollgateid:32,Tired:32,Bagequ:16,Bagprop:16,Bagmat:16,ArenanameL_:16,Arenaname:ArenanameL_/binary,Picture:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 11003:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(11004, [Honor]) ->
    Data = << Honor:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 11004:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(11005, [Coin]) ->
    Data = << Coin:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 11005:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(11006, [Diamond]) ->
    Data = << Diamond:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 11006:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(11007, [Tired,Time]) ->
    Data = << Tired:32,Time:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 11007:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(11008, [Luck]) ->
    Data = << Luck:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 11008:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(11101, [State,Progress,Id]) ->
    Data = << State:8,Progress:8,Id:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 11101:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(11102, [Code]) ->
    Data = << Code:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 11102:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(11103, [Code]) ->
    Data = << Code:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 11103:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(13001, [Type,Equip,Props]) ->
    EquipAL_ = length(Equip),
    EquipF_ = fun
        ([Id1,Type1,Equip1,Level1,Sockets1,Hp1,Attack1,Defend1,Puncture1,Hit1,Dodge1,Crit1,CritPercentage1,AnitCrit1,Toughness1]) ->
                Sockets1AL_ = length(Sockets1),
    Sockets1F_ = fun
        ([Id2,Value2]) ->
            << Id2:32,Value2:32 >>;
        ({Id2,Value2}) ->
            << Id2:32,Value2:32 >>
    end,
    Sockets1R_ = list_to_binary([Sockets1F_(X)||X <- Sockets1]),
<< Id1:32,Type1:32,Equip1:32,Level1:8,Sockets1AL_:16,Sockets1R_/binary,Hp1:32,Attack1:32,Defend1:32,Puncture1:32,Hit1:32,Dodge1:32,Crit1:32,CritPercentage1:32,AnitCrit1:32,Toughness1:32 >>;
        ({Id1,Type1,Equip1,Level1,Sockets1,Hp1,Attack1,Defend1,Puncture1,Hit1,Dodge1,Crit1,CritPercentage1,AnitCrit1,Toughness1}) ->
                Sockets1AL_ = length(Sockets1),
    Sockets1F_ = fun
        ([Id2,Value2]) ->
            << Id2:32,Value2:32 >>;
        ({Id2,Value2}) ->
            << Id2:32,Value2:32 >>
    end,
    Sockets1R_ = list_to_binary([Sockets1F_(X)||X <- Sockets1]),
<< Id1:32,Type1:32,Equip1:32,Level1:8,Sockets1AL_:16,Sockets1R_/binary,Hp1:32,Attack1:32,Defend1:32,Puncture1:32,Hit1:32,Dodge1:32,Crit1:32,CritPercentage1:32,AnitCrit1:32,Toughness1:32 >>
    end,
    EquipR_ = list_to_binary([EquipF_(X)||X <- Equip]),
    PropsAL_ = length(Props),
    PropsF_ = fun
        ([Id1,Type1,Pile1]) ->
            << Id1:32,Type1:32,Pile1:8 >>;
        ({Id1,Type1,Pile1}) ->
            << Id1:32,Type1:32,Pile1:8 >>
    end,
    PropsR_ = list_to_binary([PropsF_(X)||X <- Props]),
    Data = << Type:8,EquipAL_:16,EquipR_/binary,PropsAL_:16,PropsR_/binary >>,
    Len = byte_size(Data),
    {ok, << Len:16, 13001:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(13002, [Code]) ->
    Data = << Code:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 13002:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(13005, [Code,Equid,Level,Time]) ->
    Data = << Code:8,Equid:32,Level:8,Time:16 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 13005:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(13006, [Code,Time]) ->
    Data = << Code:8,Time:16 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 13006:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(13010, [Code,Equid]) ->
    Data = << Code:8,Equid:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 13010:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(13012, [Code]) ->
    Data = << Code:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 13012:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(13021, [Code]) ->
    Data = << Code:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 13021:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(13022, [Code,Level,Id,Type]) ->
    Data = << Code:8,Level:8,Id:32,Type:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 13022:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(13024, [MagicOrbs]) ->
    MagicOrbsAL_ = length(MagicOrbs),
    MagicOrbsF_ = fun
        ([Level1,State1]) ->
            << Level1:8,State1:8 >>;
        ({Level1,State1}) ->
            << Level1:8,State1:8 >>
    end,
    MagicOrbsR_ = list_to_binary([MagicOrbsF_(X)||X <- MagicOrbs]),
    Data = << MagicOrbsAL_:16,MagicOrbsR_/binary >>,
    Len = byte_size(Data),
    {ok, << Len:16, 13024:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(13025, [Code]) ->
    Data = << Code:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 13025:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(13026, [State,Days]) ->
    Data = << State:8,Days:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 13026:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(13027, [Code,Tab,Bags]) ->
    Data = << Code:8,Tab:8,Bags:16 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 13027:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(13028, [Code]) ->
    Data = << Code:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 13028:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(13030, [Props]) ->
    PropsAL_ = length(Props),
    PropsF_ = fun
        ([Id1,Pile1]) ->
            << Id1:32,Pile1:8 >>;
        ({Id1,Pile1}) ->
            << Id1:32,Pile1:8 >>
    end,
    PropsR_ = list_to_binary([PropsF_(X)||X <- Props]),
    Data = << PropsAL_:16,PropsR_/binary >>,
    Len = byte_size(Data),
    {ok, << Len:16, 13030:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(13032, [Code]) ->
    Data = << Code:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 13032:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(13040, [Code,Diamond,Pos]) ->
    Data = << Code:8,Diamond:32,Pos:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 13040:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(13042, [Recent]) ->
    RecentAL_ = length(Recent),
    RecentF_ = fun
        ([Name1,Reward_id1,Reward_num1]) ->
                Name1L_ = byte_size(Name1),<< Name1L_:16,Name1:Name1L_/binary,Reward_id1:32,Reward_num1:32 >>;
        ({Name1,Reward_id1,Reward_num1}) ->
                Name1L_ = byte_size(Name1),<< Name1L_:16,Name1:Name1L_/binary,Reward_id1:32,Reward_num1:32 >>
    end,
    RecentR_ = list_to_binary([RecentF_(X)||X <- Recent]),
    Data = << RecentAL_:16,RecentR_/binary >>,
    Len = byte_size(Data),
    {ok, << Len:16, 13042:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(13044, [Recent]) ->
    RecentAL_ = length(Recent),
    RecentF_ = fun
        ([Name1,Values1,Sum1]) ->
                Name1L_ = byte_size(Name1),<< Name1L_:16,Name1:Name1L_/binary,Values1:32,Sum1:32 >>;
        ({Name1,Values1,Sum1}) ->
                Name1L_ = byte_size(Name1),<< Name1L_:16,Name1:Name1L_/binary,Values1:32,Sum1:32 >>
    end,
    RecentR_ = list_to_binary([RecentF_(X)||X <- Recent]),
    Data = << RecentAL_:16,RecentR_/binary >>,
    Len = byte_size(Data),
    {ok, << Len:16, 13044:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(13045, [Id,Luck,Values]) ->
    Data = << Id:8,Luck:32,Values:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 13045:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(14001, [Status]) ->
    Data = << Status:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 14001:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(14002, [Heroes]) ->
    HeroesAL_ = length(Heroes),
    HeroesF_ = fun
        ([Id1,Type1,Seat1,Quality1,Level1,Exp1,Hp1,Attack1,Defend1,Puncture1,Hit1,Dodge1,Crit1,CritPercentage1,AnitCrit1,Toughness1]) ->
            << Id1:32,Type1:32,Seat1:8,Quality1:8,Level1:8,Exp1:32,Hp1:32,Attack1:32,Defend1:32,Puncture1:32,Hit1:32,Dodge1:32,Crit1:32,CritPercentage1:32,AnitCrit1:32,Toughness1:32 >>;
        ({Id1,Type1,Seat1,Quality1,Level1,Exp1,Hp1,Attack1,Defend1,Puncture1,Hit1,Dodge1,Crit1,CritPercentage1,AnitCrit1,Toughness1}) ->
            << Id1:32,Type1:32,Seat1:8,Quality1:8,Level1:8,Exp1:32,Hp1:32,Attack1:32,Defend1:32,Puncture1:32,Hit1:32,Dodge1:32,Crit1:32,CritPercentage1:32,AnitCrit1:32,Toughness1:32 >>
    end,
    HeroesR_ = list_to_binary([HeroesF_(X)||X <- Heroes]),
    Data = << HeroesAL_:16,HeroesR_/binary >>,
    Len = byte_size(Data),
    {ok, << Len:16, 14002:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(14010, [Code]) ->
    Data = << Code:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 14010:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(14015, [Heroes]) ->
    HeroesAL_ = length(Heroes),
    HeroesR_ = list_to_binary([<<X:32>>||X <- Heroes]),
    Data = << HeroesAL_:16,HeroesR_/binary >>,
    Len = byte_size(Data),
    {ok, << Len:16, 14015:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(14019, [Code]) ->
    Data = << Code:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 14019:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(14020, [Code,Cd,Heroes]) ->
    HeroesAL_ = length(Heroes),
    HeroesF_ = fun
        ([Id1,Type1,Lock1,Quality1,Ravity1]) ->
            << Id1:8,Type1:32,Lock1:8,Quality1:8,Ravity1:8 >>;
        ({Id1,Type1,Lock1,Quality1,Ravity1}) ->
            << Id1:8,Type1:32,Lock1:8,Quality1:8,Ravity1:8 >>
    end,
    HeroesR_ = list_to_binary([HeroesF_(X)||X <- Heroes]),
    Data = << Code:8,Cd:32,HeroesAL_:16,HeroesR_/binary >>,
    Len = byte_size(Data),
    {ok, << Len:16, 14020:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(14022, [Code]) ->
    Data = << Code:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 14022:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(14023, [Code]) ->
    Data = << Code:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 14023:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(14025, [Id,Type,Seat,Quality,Level,Exp,Hp,Attack,Defend,Puncture,Hit,Dodge,Crit,CritPercentage,AnitCrit,Toughness]) ->
    Data = << Id:32,Type:32,Seat:8,Quality:8,Level:8,Exp:32,Hp:32,Attack:32,Defend:32,Puncture:32,Hit:32,Dodge:32,Crit:32,CritPercentage:32,AnitCrit:32,Toughness:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 14025:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(14030, [Code,Heroid,Level,Exp]) ->
    Data = << Code:8,Heroid:32,Level:8,Exp:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 14030:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(22002, [Tried,CurrentCheckPoint,Success,BattleCommands,Upgrade]) ->
    BattleCommandsAL_ = length(BattleCommands),
    BattleCommandsF_ = fun
        ([Sponsor1,Buffid1,Targets1,Skill1]) ->
                Buffid1AL_ = length(Buffid1),
    Buffid1R_ = list_to_binary([<<X:32>>||X <- Buffid1]),
    Targets1AL_ = length(Targets1),
    Targets1F_ = fun
        ([Id2,Hp2,State2,Buffid2]) ->
                Buffid2AL_ = length(Buffid2),
    Buffid2R_ = list_to_binary([<<X:32>>||X <- Buffid2]),
<< Id2:8,Hp2:32,State2:8,Buffid2AL_:16,Buffid2R_/binary >>;
        ({Id2,Hp2,State2,Buffid2}) ->
                Buffid2AL_ = length(Buffid2),
    Buffid2R_ = list_to_binary([<<X:32>>||X <- Buffid2]),
<< Id2:8,Hp2:32,State2:8,Buffid2AL_:16,Buffid2R_/binary >>
    end,
    Targets1R_ = list_to_binary([Targets1F_(X)||X <- Targets1]),
<< Sponsor1:8,Buffid1AL_:16,Buffid1R_/binary,Targets1AL_:16,Targets1R_/binary,Skill1:32 >>;
        ({Sponsor1,Buffid1,Targets1,Skill1}) ->
                Buffid1AL_ = length(Buffid1),
    Buffid1R_ = list_to_binary([<<X:32>>||X <- Buffid1]),
    Targets1AL_ = length(Targets1),
    Targets1F_ = fun
        ([Id2,Hp2,State2,Buffid2]) ->
                Buffid2AL_ = length(Buffid2),
    Buffid2R_ = list_to_binary([<<X:32>>||X <- Buffid2]),
<< Id2:8,Hp2:32,State2:8,Buffid2AL_:16,Buffid2R_/binary >>;
        ({Id2,Hp2,State2,Buffid2}) ->
                Buffid2AL_ = length(Buffid2),
    Buffid2R_ = list_to_binary([<<X:32>>||X <- Buffid2]),
<< Id2:8,Hp2:32,State2:8,Buffid2AL_:16,Buffid2R_/binary >>
    end,
    Targets1R_ = list_to_binary([Targets1F_(X)||X <- Targets1]),
<< Sponsor1:8,Buffid1AL_:16,Buffid1R_/binary,Targets1AL_:16,Targets1R_/binary,Skill1:32 >>
    end,
    BattleCommandsR_ = list_to_binary([BattleCommandsF_(X)||X <- BattleCommands]),
    UpgradeAL_ = length(Upgrade),
    UpgradeF_ = fun
        ([Id1,Level1,Exp1]) ->
            << Id1:32,Level1:8,Exp1:32 >>;
        ({Id1,Level1,Exp1}) ->
            << Id1:32,Level1:8,Exp1:32 >>
    end,
    UpgradeR_ = list_to_binary([UpgradeF_(X)||X <- Upgrade]),
    Data = << Tried:32,CurrentCheckPoint:16,Success:8,BattleCommandsAL_:16,BattleCommandsR_/binary,UpgradeAL_:16,UpgradeR_/binary >>,
    Len = byte_size(Data),
    {ok, << Len:16, 22002:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(22010, [Gate,Num,Time]) ->
    Data = << Gate:16,Num:8,Time:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 22010:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(23001, [Code]) ->
    Data = << Code:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 23001:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(23004, [Rank,Point,Honor,Level]) ->
    Data = << Rank:32,Point:32,Honor:32,Level:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 23004:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(23012, [Code]) ->
    Data = << Code:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 23012:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(23014, [Lists]) ->
    ListsAL_ = length(Lists),
    ListsF_ = fun
        ([Id1,Name1]) ->
                Name1L_ = byte_size(Name1),<< Id1:32,Name1L_:16,Name1:Name1L_/binary >>;
        ({Id1,Name1}) ->
                Name1L_ = byte_size(Name1),<< Id1:32,Name1L_:16,Name1:Name1L_/binary >>
    end,
    ListsR_ = list_to_binary([ListsF_(X)||X <- Lists]),
    Data = << ListsAL_:16,ListsR_/binary >>,
    Len = byte_size(Data),
    {ok, << Len:16, 23014:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(23015, [Number,Cd,Chance,Team1,Team2,Targets]) ->
    TargetsAL_ = length(Targets),
    TargetsF_ = fun
        ([Id1,Name1,Picture1,Beat1]) ->
                Name1L_ = byte_size(Name1),<< Id1:32,Name1L_:16,Name1:Name1L_/binary,Picture1:8,Beat1:8 >>;
        ({Id1,Name1,Picture1,Beat1}) ->
                Name1L_ = byte_size(Name1),<< Id1:32,Name1L_:16,Name1:Name1L_/binary,Picture1:8,Beat1:8 >>
    end,
    TargetsR_ = list_to_binary([TargetsF_(X)||X <- Targets]),
    Data = << Number:8,Cd:32,Chance:8,Team1:8,Team2:8,TargetsAL_:16,TargetsR_/binary >>,
    Len = byte_size(Data),
    {ok, << Len:16, 23015:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(23016, [Lists]) ->
    ListsAL_ = length(Lists),
    ListsF_ = fun
        ([Index1,Id1,Name1,Exp1,Fighting1]) ->
                Name1L_ = byte_size(Name1),<< Index1:16,Id1:32,Name1L_:16,Name1:Name1L_/binary,Exp1:16,Fighting1:32 >>;
        ({Index1,Id1,Name1,Exp1,Fighting1}) ->
                Name1L_ = byte_size(Name1),<< Index1:16,Id1:32,Name1L_:16,Name1:Name1L_/binary,Exp1:16,Fighting1:32 >>
    end,
    ListsR_ = list_to_binary([ListsF_(X)||X <- Lists]),
    Data = << ListsAL_:16,ListsR_/binary >>,
    Len = byte_size(Data),
    {ok, << Len:16, 23016:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(23017, [Code]) ->
    Data = << Code:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 23017:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(23018, [Active]) ->
    ActiveAL_ = length(Active),
    ActiveF_ = fun
        ([Id1,Name11,Name21]) ->
                Name11L_ = byte_size(Name11),    Name21L_ = byte_size(Name21),<< Id1:32,Name11L_:16,Name11:Name11L_/binary,Name21L_:16,Name21:Name21L_/binary >>;
        ({Id1,Name11,Name21}) ->
                Name11L_ = byte_size(Name11),    Name21L_ = byte_size(Name21),<< Id1:32,Name11L_:16,Name11:Name11L_/binary,Name21L_:16,Name21:Name21L_/binary >>
    end,
    ActiveR_ = list_to_binary([ActiveF_(X)||X <- Active]),
    Data = << ActiveAL_:16,ActiveR_/binary >>,
    Len = byte_size(Data),
    {ok, << Len:16, 23018:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(23022, [Code,Time,Team1,Team2,Targets]) ->
    TargetsAL_ = length(Targets),
    TargetsF_ = fun
        ([Id1,Name1,Picture1,Beat1]) ->
                Name1L_ = byte_size(Name1),<< Id1:32,Name1L_:16,Name1:Name1L_/binary,Picture1:8,Beat1:8 >>;
        ({Id1,Name1,Picture1,Beat1}) ->
                Name1L_ = byte_size(Name1),<< Id1:32,Name1L_:16,Name1:Name1L_/binary,Picture1:8,Beat1:8 >>
    end,
    TargetsR_ = list_to_binary([TargetsF_(X)||X <- Targets]),
    Data = << Code:8,Time:32,Team1:8,Team2:8,TargetsAL_:16,TargetsR_/binary >>,
    Len = byte_size(Data),
    {ok, << Len:16, 23022:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(23024, [Code,Num]) ->
    Data = << Code:8,Num:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 23024:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(23025, [Code,Messege]) ->
    MessegeAL_ = length(Messege),
    MessegeF_ = fun
        ([Type1,Level1,Hp1,Seat1,Weapon1]) ->
            << Type1:32,Level1:16,Hp1:32,Seat1:16,Weapon1:8 >>;
        ({Type1,Level1,Hp1,Seat1,Weapon1}) ->
            << Type1:32,Level1:16,Hp1:32,Seat1:16,Weapon1:8 >>
    end,
    MessegeR_ = list_to_binary([MessegeF_(X)||X <- Messege]),
    Data = << Code:8,MessegeAL_:16,MessegeR_/binary >>,
    Len = byte_size(Data),
    {ok, << Len:16, 23025:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(23028, [Code,Gold,Honor]) ->
    Data = << Code:8,Gold:32,Honor:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 23028:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(23029, [Code,Gold,Honor,Point]) ->
    Data = << Code:8,Gold:32,Honor:32,Point:16 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 23029:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(23033, [Gold,Add_exp,Lev,Exp]) ->
    Data = << Gold:32,Add_exp:32,Lev:8,Exp:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 23033:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(24001, [Id]) ->
    Data = << Id:32 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 24001:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(24004, [Code,Id,Type]) ->
    Data = << Code:8,Id:16,Type:8 >>,
    Len = byte_size(Data),
    {ok, << Len:16, 24004:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(24006, [Ids]) ->
    IdsAL_ = length(Ids),
    IdsF_ = fun
        ([Id1,Type1]) ->
            << Id1:32,Type1:8 >>;
        ({Id1,Type1}) ->
            << Id1:32,Type1:8 >>
    end,
    IdsR_ = list_to_binary([IdsF_(X)||X <- Ids]),
    Data = << IdsAL_:16,IdsR_/binary >>,
    Len = byte_size(Data),
    {ok, << Len:16, 24006:16, Data/binary >>};

p(Cmd, []) -> {ok, <<0:16, Cmd:16>>};

p(Cmd, Data) -> 
    io:format("undefined_pack_cmd:~w, data:~w", [Cmd, Data]),
    {error, undefined_pack_cmd}.

%% vim: ft=erlang :
