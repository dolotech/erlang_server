%%----------------------------------------------------
%% hero
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-record(hero, {
        id
        ,tid = 0
        ,job = 0
        ,sort = 0
        ,changed = 0
        ,rare = 0 %% 稀有度
        % --- Base ---
        ,hp        = 0 %% 血
        ,atk       = 0 %% 攻
        ,def       = 0 %% 防
        ,pun       = 0 %% 穿刺
        ,hit       = 0 %% 命中
        ,dod       = 0 %% 闪避
        ,crit      = 0 %% 暴击率
        ,crit_num  = 0 %% 暴击提成
        ,crit_anti = 0 %% 免暴
        ,tou       = 0 %% 韧性
        % --- ---- ---
        ,tou_anit  = 0 %% 免韧
        ,pos = 0
        ,exp_max = 0
        ,exp = 0
        ,lev = 0
        ,step = 0
        %% 品极
        %% 1 = d
        %% 2 = c
        %% 3 = b
        %% 4 = a
        %% 5 = s
        %% 6 = ss
        %% 7 = sss
        ,quality = 1
        ,skills = [] %% [{Pos, Id, Rate, Tags}, ...]
    }
).
