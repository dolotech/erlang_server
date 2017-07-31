%%----------------------------------------------------
%% 角色相关数据结构定义
%%
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

%% 角色进程状态数据结构
-record(role, {
        % --- int ---
         id             = 0     %% 角色ID
        ,sex
        ,growth         = 0     %% 0未创建角色,1已创建,2已选英雄
        ,lev            = 1
        ,exp            = 0
        ,exp_max        = 0
        ,gold           = 0     %% 金币
        ,diamond        = 0
        ,essence        = 0     %% 精华值
        ,status         = 0     %% 0=末登陆,1=登陆
        ,max_hero_id    = 0
        ,max_item_id    = 0
        ,tollgate_id    = 1     %% 关卡ID
        ,power          = 100     %%
        ,power_time     = 0     %% 上次计算时间
        ,sign           = 0     %% 32位布尔标记
        %% 英雄酒馆刷新时间
        %% 未初始化时值为undefined
        ,tavern_time    = undefined
        ,sign_days      = 0     %% 签到天数
        ,sign_time      = 0     %% 签到时间
        ,sign_old_days  = 0     %% 已经签到天数
		,bag_equ_max    = 16    %% 装备背包格子数，默认2行14个
		,bag_prop_max   = 16    %% 道具背包格子数，默认2行14个
		,bag_mat_max    = 16    %% 材料背包格子数，默认2行14个
        ,fb_combat1     = 0     %% 副本战斗当前可用次数（初）
        ,fb_combat2     = 0     %% 副本战斗当前可用次数（中）
        ,fb_combat3     = 0     %% 副本战斗当前可用次数（高）
        ,fb_time1		= 0     %% 上次刷新时间（初）
        ,fb_time2		= 0     %% 上次刷新时间（中）
        ,fb_time3		= 0     %% 上次刷新时间（高）
        ,fb_gate		= undefined   %% 副本关卡 {Type, 副本关卡ID}
        % --- ~~ ---
        ,arena_id       = 0     %% 竞技ID
		,arena_lev		= 1		%% 竞技场段位
		,arena_exp		= 0		%% 竞技场积分
        ,arena_rank		= 0		%% 竞技场排名(0表示没有上榜)
		,arena_picture  = 1     %% 竞技场头像
        % --- ~~ ---
		,arena_time		= 0		%% 竞技场上次刷新时间
		,arena_honor	= 0		%% 竞技场荣誉值
		% --- ~~ ---
		,arena_wars		= 20	%% 竞技场挑战次数
		,arena_chance	= 0		%% 竞技场购买挑战机会次数
		,arena_rank_box	= 0		%% 竞技场排行榜宝箱(0未领取,1已领取)
		,arena_revenge  = 0		%% 竞技场悬赏次数
		,arena_prize	= 0		%% 竞技场揭榜次数
		% --- ~~ ---
		,arena_combat_box1 = 0	%% 竞技场挑战宝箱(0未领取,1已领取)
		,arena_combat_box2 = 0	%% 竞技场挑战宝箱(0未领取,1已领取)
        % --- pid ---
        ,pid            = 0     %% 角色主进程ID
        ,pid_conn       = 0     %% 连接处理进程ID
        ,pid_sender     = 0     %% Socket数据发包进程
        % --- port ---
        ,socket         = 0     %% socket
        ,port
        % --- binary ---
        ,ip
        ,name           = <<>>  %% 角色名称
        ,account_id
        ,password       = <<>>  %% 密码
        % --- tuple ---
		,luck	   = {110, 0, 0, 0}	%% = {幸运星,幸运钻石,累计使用幸运星, 总价值}
        ,produce_pass %%    = {Type, PassId}     %% 当前掉落关卡
        ,fb  %% #fb
        % --- list ---
        ,save   = []

        %% ------------------------------------------------------------------
        %% save_delay: 延迟回存标识，
        %% 如果在save里的内容保存失败，
        %% 将转到这里，进程退出时再尝试保存
        %% ------------------------------------------------------------------
        ,save_delay = [] 

        ,heroes = []
        ,items  = []

        %% ------------------------------------------------------------------
        %% jewel: 宝珠抽取状态
        %% 如为[]时会初始化为: [{1, 1}, {2, 0}, {3, 0}, {4, 0}, {5, 0}]
        %% 格式: [{Quality, Status}, ...]
        %%       Quality : 宝珠品质等级
        %%       Status : 是否开始(1=是,0=否)
        %% ------------------------------------------------------------------
        ,jewel  = []

        ,tavern = [] %% [{Id, IsLock, #hero}]
		,arena	= [] %% [[Id, name, picture, IsBeat]] %% IsBeat:0=未击败, 1=已击败
		,attain = [] %% [{Id, NextId, Type, Condition, State}] %% State:0=未完成, 1=可领取, 2=全部完成
        ,arena_lost_report = []
    }
).
