-module(data_item).
-export([get/1]).
get(ids) -> [30001,30008,30003,30033,20001,20002,20003,20004,40002,40004,40001];
get(30001) -> [{job, 1}, {sort, 1}, {atk, [52,78]}, {hp, [308,692]}, {agi, [25,40]}];
get(30008) -> [{job, 1}, {sort, 1}, {atk, [62,92]}, {hp, [260,584]}, {agi, [25,40]}];
get(30003) -> [{job, 2}, {sort, 1}, {atk, [86,130]}, {hp, [274,411]}, {agi, [35,50]}];
get(30033) -> [{job, 3}, {sort, 1}, {atk, [112,168]}, {hp, [286,321]}, {agi, [15,30]}];
get(20001) -> [{job, 1}, {sort, 2}, {atk, 999}, {hp, 0}, {lve_min, 1}];
get(20002) -> [{job, 1}, {sort, 3}, {atk, 20}, {hp, 150}, {lve_min, 3}];
get(20003) -> [{job, 1}, {sort, 4}, {atk, 0}, {hp, 5000}, {lve_min, 5}];
get(20004) -> [{job, 1}, {sort, 5}, {atk, 30}, {hp, 100}, {lve_min, 8}];
get(40002) -> [{job, 3}, {sort, 6}, {quality, 1}, {lve_min, 1}, {ctl1, 50002}];
get(40004) -> [{job, 2}, {sort, 6}, {quality, 1}, {lve_min, 1}, {ctl1, 50004}];
get(40001) -> [{job, 1}, {sort, 6}, {quality, 1}, {lve_min, 1}, {ctl1, 50001}];
get(_) -> undefined.