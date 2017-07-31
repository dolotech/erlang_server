-module(data_produce_num).
-export([get/1]).
get(ids) -> [1,2,3,4,5];
get(1) -> [{rand_top, 50}, {range, [{1,1,50}]}];
get(2) -> [{rand_top, 75}, {range, [{1,1,50},{2,51,75}]}];
get(3) -> [{rand_top, 87}, {range, [{1,1,50},{2,51,75},{3,76,87}]}];
get(4) -> [{rand_top, 95}, {range, [{1,1,50},{2,51,75},{3,76,87},{4,88,95}]}];
get(5) -> [{rand_top, 100}, {range, [{1,1,50},{2,51,75},{3,76,87},{4,88,95},{5,96,100}]}];
get(_) -> undefined.