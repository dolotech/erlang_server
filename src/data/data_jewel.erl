-module(data_jewel).
-export([get/1]).
get(ids) -> [1,2,3,4,5];
get(1) -> [{rate_next, 80}, {rate, 100}, {rate2, 0}, {money, 2}, {num, 5}, {money1, 1}, {num1, 3000}];
get(2) -> [{rate_next, 60}, {rate, 60}, {rate2, 100}, {money, 2}, {num, 25}, {money1, 1}, {num1, 8000}];
get(3) -> [{rate_next, 45}, {rate, 40}, {rate2, 100}, {money, 2}, {num, 50}, {money1, 1}, {num1, 15000}];
get(4) -> [{rate_next, 25}, {rate, 20}, {rate2, 100}, {money, 2}, {num, 70}, {money1, 1}, {num1, 30000}];
get(5) -> [{rate_next, 0}, {rate, 8}, {rate2, 100}, {money, 2}, {num, 90}, {money1, 1}, {num1, 50000}];
get(_) -> undefined.