%%----------------------------------------------------
%% ROBOT
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

%% 角色AI
-define(ACT_MOVE, 0).
-define(ACT_HIT, 4).

-define(DIR_LEFT, 37).
-define(DIR_UP, 38).
-define(DIR_RIGHT, 39).
-define(DIR_DOWN, 40).

-define(TYPE_FIX, 1).
-define(TYPE_STOP, 2).
-define(TYPE_GO, 98).
-define(TYPE_SKIP, 99).

%% 测试用：角色进程状态数据结构
-record(rs, {
        id
        ,sex = 0
        ,acc_id
        ,pid
        ,pid_sender
        ,pid_ai
        ,socket
        ,map_id = 0
        ,type %% 游戏模式
        ,move_speed
        ,hp        = 0
        ,speed_row = 100
        ,speed_col = 100
        ,speed_hit = 800
        ,hit_delay = 0
        ,dmg_speed
        ,x = 0
        ,y = 0
        ,team = 0
        ,dir
        ,last_dir
        ,move_ref
        ,enemy
        ,find_enemy = 1 %% 是否主动寻找敌方(0 | 1)
        ,find_npc = 1 %% 是否主动寻找敌方(0 | 1)
        ,is_host = 0
        ,is_battle = 0 %% 是否战斗中
        ,is_invited = 0 %% 是否被邀请进入游戏的
        ,trigger
        ,avg_lev = 0
        ,lev = 0
        ,ai
        ,ip
        ,port
        ,guild_id = 0
        ,gold = 0
        ,card = 0
        ,action %% 要执行的一系列动作
        ,status = 1
        ,room = [] %% [{Id, Status}, ...]
        ,skills = []
        ,name = <<>>
        ,tmp_package = <<>>
        ,to_room_id = 0 %% 对手的房间号
        ,must_lost = 0
        ,myscore = 0
        ,opscore = 0
    }
).

%% AI设定
-record(ai, {
        hit_front = 100 %% 敲击动作
        ,hit_back = 100 %% 敲击动作
        ,hit_up = 100 %% 敲击动作
        ,hit_down = 100 %% 敲击动作
        ,hit_reaction = [0, 0] %% 敲击后的反应时间范围
        ,find_enemy = 100 %% 寻找敌人的概率
        ,find_npc = 100 %% 寻找敌人的概率
        ,leave_enemy = 100 %% 地板快碎时离开敌人的概率
        ,use_daoju = 100 %% 使用的概率
    }
).
