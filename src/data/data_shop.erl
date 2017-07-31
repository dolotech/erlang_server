-module(data_shop).
-export([get/1]).
get(ids) -> [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,18,19,20];
get(1) -> [{tid, 10107}, {num1, 1}, {price1, 220}];
get(2) -> [{tid, 10108}, {num1, 1}, {price1, 440}];
get(3) -> [{tid, 10109}, {num1, 1}, {price1, 880}];
get(4) -> [{tid, 10107}, {num1, 5}, {price1, 1080}];
get(5) -> [{tid, 10108}, {num1, 5}, {price1, 2160}];
get(6) -> [{tid, 10109}, {num1, 5}, {price1, 4320}];
get(7) -> [{tid, 12015}, {num1, 1}, {price1, 1500}];
get(8) -> [{tid, 12025}, {num1, 1}, {price1, 1500}];
get(9) -> [{tid, 12035}, {num1, 1}, {price1, 1500}];
get(10) -> [{tid, 12045}, {num1, 1}, {price1, 1200}];
get(11) -> [{tid, 12055}, {num1, 1}, {price1, 1200}];
get(12) -> [{tid, 12065}, {num1, 1}, {price1, 1200}];
get(13) -> [{tid, 12075}, {num1, 1}, {price1, 1200}];
get(14) -> [{tid, 12085}, {num1, 1}, {price1, 1200}];
get(15) -> [{tid, 12095}, {num1, 1}, {price1, 1200}];
get(18) -> [{tid, 3}, {num1, 1}, {price1, 20}];
get(19) -> [{tid, 3}, {num1, 5}, {price1, 95}];
get(20) -> [{tid, 3}, {num1, 10}, {price1, 180}];
get(_) -> undefined.