-module(data_fb).
-export([get/1]).
get(ids) -> [1,2,3,4,5,6];
get(1) -> [{elite_time, 10}, {boss_time, 10}, {big_boss_time, 10}];
get(2) -> [{elite_time, 10}, {boss_time, 10}, {big_boss_time, 10}];
get(3) -> [{elite_time, 10}, {boss_time, 10}, {big_boss_time, 10}];
get(4) -> [{elite_time, 10}, {boss_time, 10}, {big_boss_time, 10}];
get(5) -> [{elite_time, 10}, {boss_time, 10}, {big_boss_time, 10}];
get(6) -> [{elite_time, 10}, {boss_time, 10}, {big_boss_time, 10}];
get(_) -> undefined.