-module(data_config).
-export([get/1]).
get(ids) -> [];
get(init_gold) -> 10000;
get(init_diamond) -> 50;
get(tired_max) -> 100;
get(diamond_per_min) -> 0.5;
get(refresh_time) -> 9000;
get(qifuxiaohao) -> 10;
get(qifu) -> 2000;
get(total_exp) -> 9412860;
get(arenaCd) -> 2400;
get(arena_revenge) -> 3;
get(arenaBuy) -> 5;
get(fb_max1) -> 15;
get(fb_max2) -> 7;
get(fb_max3) -> 4;
get(fb_cd1) -> 60;
get(fb_cd2) -> 1800;
get(fb_cd3) -> 7200;
get(starCumulative) -> 10;
get(starBack) -> 800;
get(_) -> undefined.