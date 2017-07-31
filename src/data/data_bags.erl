-module(data_bags).
-export([get/1]).
get(ids) -> [1,2,3,4,5,6,7,8,9,10];
get(1) -> [{price, 5}];
get(2) -> [{price, 10}];
get(3) -> [{price, 15}];
get(4) -> [{price, 20}];
get(5) -> [{price, 50}];
get(6) -> [{price, 80}];
get(7) -> [{price, 120}];
get(8) -> [{price, 150}];
get(9) -> [{price, 300}];
get(10) -> [{price, 500}];
get(_) -> undefined.