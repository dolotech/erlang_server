-module(data_hero_give).
-export([get/1]).
get(ids) -> [30005,30003,30004,30002];
get(30005) -> [{quality, 1}, {arg, 0.33}, {rare, 1}];
get(30003) -> [{quality, 1}, {arg, 0.33}, {rare, 1}];
get(30004) -> [{quality, 1}, {arg, 0.33}, {rare, 1}];
get(30002) -> [{quality, 1}, {arg, 0.33}, {rare, 1}];
get(_) -> undefined.