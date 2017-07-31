-module(data_sign).
-export([get/1]).
get(ids) -> [1,2,4,5,6,8,10];
get(1) -> [{coin, 10000}, {diamond, 10}, {tid_num, [{10101,5}]}];
get(2) -> [{coin, 15000}, {diamond, 50}, {tid_num, [{10102,5}]}];
get(4) -> [{coin, 20000}, {diamond, 100}, {tid_num, [{10103,5}]}];
get(5) -> [{coin, 25000}, {diamond, 120}, {tid_num, [{10104,5}]}];
get(6) -> [{coin, 30000}, {diamond, 150}, {tid_num, [{10105,5}]}];
get(8) -> [{coin, 35000}, {diamond, 200}, {tid_num, [{10105,15}]}];
get(10) -> [{coin, 99999}, {diamond, 666}, {tid_num, [{10105,30}]}];
get(_) -> undefined.