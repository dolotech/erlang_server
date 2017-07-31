-module(data_arena_exp).
-export([get/1]).
get(ids) -> [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27];
get(1) -> [{exp_max, 81}];
get(2) -> [{exp_max, 118}];
get(3) -> [{exp_max, 167}];
get(4) -> [{exp_max, 234}];
get(5) -> [{exp_max, 325}];
get(6) -> [{exp_max, 446}];
get(7) -> [{exp_max, 603}];
get(8) -> [{exp_max, 802}];
get(9) -> [{exp_max, 1049}];
get(10) -> [{exp_max, 1350}];
get(11) -> [{exp_max, 1711}];
get(12) -> [{exp_max, 2138}];
get(13) -> [{exp_max, 2637}];
get(14) -> [{exp_max, 3214}];
get(15) -> [{exp_max, 3875}];
get(16) -> [{exp_max, 4626}];
get(17) -> [{exp_max, 5473}];
get(18) -> [{exp_max, 6422}];
get(19) -> [{exp_max, 7479}];
get(20) -> [{exp_max, 8650}];
get(21) -> [{exp_max, 9941}];
get(22) -> [{exp_max, 11358}];
get(23) -> [{exp_max, 12907}];
get(24) -> [{exp_max, 14594}];
get(25) -> [{exp_max, 16425}];
get(26) -> [{exp_max, 18406}];
get(27) -> [{exp_max, 0}];
get(_) -> undefined.