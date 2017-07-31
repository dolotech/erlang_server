%% $Id: item.hrl 4242 2013-11-04 03:34:49Z rolong $

%% 个人物品信息
-record(item, {
        id
        ,tid
        ,sort = 0
        ,tab = 0 %% 1＝材料，2＝道具，5＝装备
        ,changed = 0
        ,attr
    }).
