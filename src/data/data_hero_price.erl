-module(data_hero_price).
-export([get/1]).
get(ids) -> [{1,1},{1,2},{1,3},{1,4},{1,5},{1,6},{1,7},{2,1},{2,2},{2,3},{2,4},{2,5},{2,6},{2,7},{3,1},{3,2},{3,3},{3,4},{3,5},{3,6},{3,7},{4,1},{4,2},{4,3},{4,4},{4,5},{4,6},{4,7},{5,1},{5,2},{5,3},{5,4},{5,5},{5,6},{5,7}];
get({1,1}) -> [{type, 2}, {price, 10}];
get({1,2}) -> [{type, 2}, {price, 50}];
get({1,3}) -> [{type, 2}, {price, 80}];
get({1,4}) -> [{type, 2}, {price, 150}];
get({1,5}) -> [{type, 2}, {price, 300}];
get({1,6}) -> [{type, 2}, {price, 600}];
get({1,7}) -> [{type, 2}, {price, 1200}];
get({2,1}) -> [{type, 2}, {price, 10}];
get({2,2}) -> [{type, 2}, {price, 60}];
get({2,3}) -> [{type, 2}, {price, 95}];
get({2,4}) -> [{type, 2}, {price, 180}];
get({2,5}) -> [{type, 2}, {price, 360}];
get({2,6}) -> [{type, 2}, {price, 720}];
get({2,7}) -> [{type, 2}, {price, 1440}];
get({3,1}) -> [{type, 2}, {price, 10}];
get({3,2}) -> [{type, 2}, {price, 70}];
get({3,3}) -> [{type, 2}, {price, 110}];
get({3,4}) -> [{type, 2}, {price, 215}];
get({3,5}) -> [{type, 2}, {price, 430}];
get({3,6}) -> [{type, 2}, {price, 860}];
get({3,7}) -> [{type, 2}, {price, 1725}];
get({4,1}) -> [{type, 2}, {price, 10}];
get({4,2}) -> [{type, 2}, {price, 80}];
get({4,3}) -> [{type, 2}, {price, 130}];
get({4,4}) -> [{type, 2}, {price, 255}];
get({4,5}) -> [{type, 2}, {price, 515}];
get({4,6}) -> [{type, 2}, {price, 1030}];
get({4,7}) -> [{type, 2}, {price, 2070}];
get({5,1}) -> [{type, 2}, {price, 10}];
get({5,2}) -> [{type, 2}, {price, 95}];
get({5,3}) -> [{type, 2}, {price, 155}];
get({5,4}) -> [{type, 2}, {price, 305}];
get({5,5}) -> [{type, 2}, {price, 615}];
get({5,6}) -> [{type, 2}, {price, 1235}];
get({5,7}) -> [{type, 2}, {price, 2480}];
get(_) -> undefined.