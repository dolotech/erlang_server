-module(data_hero).
-export([get/1]).
get(ids) -> [30001,30003,30002,30004,30005,30006,30007,30008,30009,30010,30011];
get(30001) -> [{job, 2}, {sort, 2}, {rare, 1}, {atk, 740}, {hp, 11620}, {def, 225}, {pun, 112}, {hit, 150}, {dod, 150}, {crit, 150}, {crit_num, 150}, {crit_anti, 112}, {tou, 0}, {skill1, 1}, {skill2, 2}, {skill3, 3}];
get(30003) -> [{job, 1}, {sort, 3}, {rare, 2}, {atk, 1500}, {hp, 6300}, {def, 75}, {pun, 180}, {hit, 135}, {dod, 165}, {crit, 112}, {crit_num, 262}, {crit_anti, 120}, {tou, 0}, {skill1, 7}, {skill2, 8}, {skill3, 9}];
get(30002) -> [{job, 1}, {sort, 5}, {rare, 1}, {atk, 1800}, {hp, 4200}, {def, 38}, {pun, 98}, {hit, 225}, {dod, 135}, {crit, 262}, {crit_num, 188}, {crit_anti, 105}, {tou, 0}, {skill1, 10}, {skill2, 11}, {skill3, 12}];
get(30004) -> [{job, 1}, {sort, 6}, {rare, 2}, {atk, 1680}, {hp, 5040}, {def, 75}, {pun, 262}, {hit, 225}, {dod, 60}, {crit, 150}, {crit_num, 225}, {crit_anti, 52}, {tou, 0}, {skill1, 4}, {skill2, 5}, {skill3, 6}];
get(30005) -> [{job, 1}, {sort, 1}, {rare, 5}, {atk, 1320}, {hp, 7560}, {def, 150}, {pun, 150}, {hit, 150}, {dod, 150}, {crit, 150}, {crit_num, 150}, {crit_anti, 150}, {tou, 0}, {skill1, 16}, {skill2, 17}, {skill3, 18}];
get(30006) -> [{job, 3}, {sort, 4}, {rare, 1}, {atk, 1080}, {hp, 9240}, {def, 150}, {pun, 150}, {hit, 150}, {dod, 150}, {crit, 150}, {crit_num, 150}, {crit_anti, 150}, {tou, 0}, {skill1, 25}, {skill2, 26}, {skill3, 27}];
get(30007) -> [{job, 3}, {sort, 5}, {rare, 2}, {atk, 1440}, {hp, 6720}, {def, 90}, {pun, 150}, {hit, 188}, {dod, 150}, {crit, 150}, {crit_num, 150}, {crit_anti, 172}, {tou, 0}, {skill1, 22}, {skill2, 23}, {skill3, 24}];
get(30008) -> [{job, 2}, {sort, 4}, {rare, 3}, {atk, 600}, {hp, 12600}, {def, 225}, {pun, 150}, {hit, 150}, {dod, 188}, {crit, 75}, {crit_num, 75}, {crit_anti, 188}, {tou, 0}, {skill1, 13}, {skill2, 14}, {skill3, 15}];
get(30009) -> [{job, 3}, {sort, 2}, {rare, 2}, {atk, 1740}, {hp, 4620}, {def, 90}, {pun, 188}, {hit, 188}, {dod, 90}, {crit, 188}, {crit_num, 150}, {crit_anti, 158}, {tou, 0}, {skill1, 19}, {skill2, 20}, {skill3, 21}];
get(30010) -> [{job, 4}, {sort, 5}, {rare, 1}, {atk, 1440}, {hp, 6720}, {def, 150}, {pun, 120}, {hit, 120}, {dod, 200}, {crit, 150}, {crit_num, 150}, {crit_anti, 160}, {tou, 0}, {skill1, 28}, {skill2, 29}, {skill3, 30}];
get(30011) -> [{job, 4}, {sort, 5}, {rare, 4}, {atk, 1800}, {hp, 4200}, {def, 120}, {pun, 120}, {hit, 120}, {dod, 120}, {crit, 188}, {crit_num, 150}, {crit_anti, 232}, {tou, 0}, {skill1, 31}, {skill2, 32}, {skill3, 33}];
get(_) -> undefined.