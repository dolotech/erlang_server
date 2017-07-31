%% $Id: equ.hrl 3966 2013-10-26 06:32:02Z rolong $

%% 装备属性
-record(equ, {
        hero_id    = 0 %% 物品的所属英雄
        ,etime     = 0 %% 到期时间
        ,atime     = 0 %% 允许强化时间
        ,lev       = 0
        ,quality   = 0
        % --- Base ---
        ,hp        = 0
        ,atk       = 0 %% 攻
        ,def       = 0 %% 防
        ,pun       = 0 %% 穿刺
        ,hit       = 0 %% 命中
        ,dod       = 0 %% 闪避
        ,crit      = 0 %% 暴击
        ,crit_num  = 0 %% 暴击提成
        ,crit_anti = 0 %% 免暴
        ,tou       = 0 %% 韧性
        ,tou_anit  = 0 %% 免韧
        ,sockets
        % --- ---- ---
        ,embed     = [] %% 宝珠 [{Tid, AttrId, AttrVal}]
    }).
