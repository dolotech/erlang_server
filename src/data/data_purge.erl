-module(data_purge).
-export([get/1]).
get(ids) -> [1,2,3,4,5,6];
get(1) -> [{materials, [{13001,3}]}, {type, 1}, {num, 5000}];
get(2) -> [{materials, [{13002,5}]}, {type, 1}, {num, 10000}];
get(3) -> [{materials, [{13003,10}]}, {type, 1}, {num, 20000}];
get(4) -> [{materials, [{13004,20}]}, {type, 1}, {num, 100000}];
get(5) -> [{materials, [{13005,20}]}, {type, 2}, {num, 320}];
get(6) -> [{materials, [{13006,20}]}, {type, 2}, {num, 750}];
get(_) -> undefined.