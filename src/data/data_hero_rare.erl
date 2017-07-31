-module(data_hero_rare).
-export([get/1]).
get(ids) -> [{1,20},{21,37},{38,51},{52,62},{63,70}];
get({1,20}) -> {1};
get({21,37}) -> {2};
get({38,51}) -> {3};
get({52,62}) -> {4};
get({63,70}) -> {5};
get(range) -> {1, 70};
get(_) -> undefined.