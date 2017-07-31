-module(data_fun).
-export([
     power_speed/0,
     calc_crit_num/1,
     calc_hit_rate/2,
     calc_active_cure/4,
     up_spend/1,
     fb_spend/1,
     hero_offset/0,
     equ_offset/0,
     jewel_offset/0
]).

power_speed() -> X = data_config:get(tired_max), round(1440/X*4).
calc_crit_num(X) -> Y = min(X, 1000),  1.5-4.5*(math:pow(10, -6)*Y*Y-2*math:pow(10, -3)*Y).
calc_hit_rate(DodB,HitA) -> X = min(DodB - HitA * 1.2, 1000),  1+0.5*(math:pow(10, -6)*X*X-2*math:pow(10, -3)*X).
calc_active_cure(Atk,Crit,Dmg,Arg) -> Atk*Crit*Dmg*Arg.
up_spend(X) -> X*0.2+500.
fb_spend(X) -> util:floor(0.6*X*X-2*X+3).
hero_offset() -> X=util:rand(1, 1000), (X*X/10000*0.1+90) / 100.
equ_offset() -> X=util:rand(1, 1000), (X*X/10000*0.1+90) / 100.
jewel_offset() -> X=util:rand(1, 1000), (X*X/10000*0.1+90) / 100.
