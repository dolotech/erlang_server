%%----------------------------------------------------
%% 协议11 - 角色相关
%%
%% $id$
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-module(pt_role).
-export([handle/3]).

-include("common.hrl").

%% 帐号方式 登陆/注册
%%
%% 登陆/注册验证说明：
%%
%% 服务器ID(sid) ：1
%% 随机数(rand)  ：0~255的一个随机数字
%% 当前加密KEY   ：23d7f859778d2093
%% 签名算法      ：md5(sid + rand + key + account)
%%
%% TODO:
%%     1、密码要MD5后保存
%%     2、随机数改为当前时间，每次登陆时间不能一样
%%     3、不同的服务器需要不同的KEY
%%     4、修改密码功能
%%
handle(11000, [Type, Rand, Sid, Aid, Password, Signature], Rs) ->
    ?DEBUG("LOGIN(11000): ~w, ~w, ~w, ~s, ~s, ~s", [Type, Rand, Sid, Aid, Password, Signature]),
    SignatureChkStr = list_to_binary([
            integer_to_list(Sid)
            ,integer_to_list(Rand)
            ,env:get(server_key)
            ,Aid
        ]),
    SignatureChk = util:md5(SignatureChkStr),
    case Signature =:= SignatureChk of
        true ->
            case Type of
                1 ->
                    %% 注册
                    case db:get_one("select count(*) from role where aid = ~s limit 1", [Aid]) of
                        {ok, 0} ->
                            Rs1 = Rs#role{
                                sex = 0 
                                ,growth = 1
                            },
                            Gold = data_config:get(init_gold),
                            Diamond = data_config:get(init_diamond),
                            Name1 = <<>>,
                            AccountId1 = ?ESC_SINGLE_QUOTES(Aid),
                            Password1 = ?ESC_SINGLE_QUOTES(Password),
                            Kvs = lib_role:zip_kvs(Rs1),
                            Lev = 1,
                            Sql = list_to_binary([
                                    <<"insert into role(gold, diamond, essence, aid, password, name, lev, tollgate_id, kvs) values(">>
                                    ,integer_to_list(Gold)
                                    ,<<",">>,integer_to_list(Diamond)
                                    ,<<",0">>
                                    ,<<",'">>,AccountId1,<<"'">>
                                    ,<<",'">>,Password1,<<"'">>
                                    ,<<",'">>,Name1,<<"'">>
                                    ,<<",">>,integer_to_list(Lev)
                                    ,<<",">>,integer_to_list(Rs#role.tollgate_id)
                                    ,<<",'">>,Kvs,<<"'">>
                                    ,<<")">>
                                ]),
                            case db:execute(Sql) of
                                {ok, 1} ->
                                    case account_login(Aid, Rs1, true) of
                                        {ok, Rid, Growth, Rs2} ->
                                            {ok, [1, Growth, Rid], Rs2};
                                        {error, Reason} ->
                                            %% 注册失败3
                                            ?WARN("REG ERROR: ~w", [Reason]),
                                            {ok, [131, 0, 0], Rs}
                                    end;
                                {error, Reason} ->
                                    %% 注册失败2
                                    ?WARN("REG ERROR: ~w", [Reason]),
                                    {ok, [130, 0, 0], Rs}
                            end;
                        {ok, _} ->
                            ?DEBUG("[Repeat Reg] account_id: ~s", [Aid]),
                            %% 帐号已存在
                            {ok, [6, 0, 0]};
                        {error, Reason} ->
                            %% 注册失败1
                            ?WARN("REG ERROR: ~w", [Reason]),
                            {ok, [129, 0, 0]}
                    end;
                2 ->
                    %% 登陆
                    case check_password(Rs, Aid, Password) of
                        {ok, Rs1} ->
                            case account_login(Aid, Rs1, false) of
                                {ok, Rid, Growth, Rs2} ->
                                    %% 登陆成功
                                    ?DEBUG("***** LOGIN GROWTH: ~w Rid:~w *****", [Growth, Rid]),
                                    {ok, [2, Growth, Rid], Rs2};
                                {error, no_reg} ->
                                    %% 帐号不存在
                                    {ok, [5, 0, 0]};
                                {error, Reason} ->
                                    %% 登陆失败2
                                    ?WARN("LOGIN ERROR: ~w", [Reason]),
                                    {ok, [4, 0, 0]}
                            end;
                        {error, error} ->
                            %% 帐号或密码不正确
                            ?DEBUG("Password ERROR", []),
                            {ok, [3, 0, 0]};
                        {error, empty} ->
                            %% 密码不能为空
                            ?DEBUG("Password ERROR", []),
                            {ok, [3, 0, 0]};
                        {error, Reason} ->
                            %% 登陆失败1
                            ?DEBUG("Password ERROR: ~w", [Reason]),
                            {ok, [3, 0, 0]}
                    end;
                Else ->
                    %% 错误的登陆方式
                    ?WARN("ERROR TYPE:~w", [Else]),
                    {ok, [128, 0, 0]}
            end;
        false ->
            %% 签名不正确
            ?DEBUG("Signature1:~s", [Signature]),
            ?DEBUG("Signature2:~s", [SignatureChk]),
            {ok, [127, 0, 0]}
    end;

%% 角色登录
%% handle(11001, [Bin], Rs) ->
%%     ?DEBUG("11001 Login:~s", [Bin]),
%%     %% ?DEBUG("Login:~w", [Bin]),
%%     Qs = util:parse_qs(Bin),
%%     case lists:keyfind(<<"account_id">>, 1, Qs) of
%%         {_, AccountId} -> 
%%             case account_login(AccountId, Rs, false) of
%%                 {ok, Rid, Growth, Rs1} ->
%%                     ?DEBUG("***** LOGIN GROWTH: ~w Rid:~w *****", [Growth, Rid]),
%%                     {ok, [0, Growth, Rid], Rs1};
%%                 {error, no_reg} ->
%%                     {ok, [0, 0, 0], Rs#role{account_id = AccountId}};
%%                 {error, _} ->
%%                     {ok, [1, 0, 0]}
%%             end;
%%         _Other -> {ok, [1, 0, 0]}
%%     end;
%% 
%% %% 创建角色
%% handle(11002, [_Name, _Sex], Rs) ->
%%     %% Name = list_to_binary(integer_to_list(util:unixtime()) ++ "_" ++ integer_to_list(util:rand(1, 99999))),
%%     Name = <<>>,
%%     Sex = 0,
%%     ?DEBUG("11002:~w", [[Name, Sex]]),
%%     #role{account_id = AccountId, ip = _Ip} = Rs,
%%     IsReged1 = db:get_one("select id from role where aid = ~s limit 1", [AccountId]),
%%     if
%%         AccountId =:= undefined -> 
%%             ?DEBUG("No login!", []),
%%             %% 没有登陆
%%             {ok, [5]};
%%         IsReged1 =/= null ->
%%             ?DEBUG("Repeat reg! account_id:~w", [AccountId]),
%%             %% 已创建角色
%%             {ok, [6]};
%%         true ->
%%             Rs1 = Rs#role{
%%                 sex = Sex 
%%                 ,growth = 1
%%             },
%%             Gold = data_config:get(init_gold),
%%             Diamond = data_config:get(init_diamond),
%%             Name1 = ?ESC_SINGLE_QUOTES(Name),
%%             AccountId1 = ?ESC_SINGLE_QUOTES(AccountId),
%%             Kvs = lib_role:zip_kvs(Rs1),
%%             Lev = 1,
%%             Sql = list_to_binary([
%%                     <<"insert into role(gold, diamond, essence, aid, name, lev, tollgate_id, kvs) values(">>
%%                     ,integer_to_list(Gold)
%%                     ,<<",">>,integer_to_list(Diamond)
%%                     ,<<",0">>
%%                     ,<<",'">>,AccountId1,<<"'">>
%%                     ,<<",'">>,Name1,<<"'">>
%%                     ,<<",">>,integer_to_list(Lev)
%%                     ,<<",">>,integer_to_list(Rs#role.tollgate_id)
%%                     ,<<",'">>,Kvs,<<"'">>
%%                     ,<<")">>
%%                 ]),
%%             db:execute(Sql),
%%             case account_login(AccountId, Rs1, true) of
%%                 {ok, Rid, Growth, Rs2} ->
%%                     sender:pack_send(Rs#role.pid_sender, 11001, 
%%                         [0, Growth, Rid]),
%%                     {ok, [0], Rs2};
%%                 {error, Reason} ->
%%                     ?ERR("REG ERROR: ~w", [Reason]),
%%                     {ok, [127], Rs}
%%             end
%%     end;

%% 角色信息
handle(11003, [], Rs) ->
    #role{diamond = Diamond, gold = Gold,
          tollgate_id = TollgateId, power = Power,
		  bag_mat_max = BagMatMax, bag_prop_max = BagPropMax, bag_equ_max = BagEquMax,
		  name = Name, arena_picture = Picture} = Rs,
    {ok, [Diamond, Gold, TollgateId, Power, BagEquMax, BagPropMax, BagMatMax, Name, Picture]};

%% 更新Power
handle(11007, [], Rs) ->
    Rs1 = lib_role:time2power(Rs),
    #role{power = Power, 
          power_time = Time} = Rs1,
    Max = data_config:get(tired_max),
    T = 1440/Max*4*60,
    Rest = util:floor(T - (util:unixtime() - Time)),
    ?DEBUG("[Power:~w, Rest:~w]", [Power, Rest]),
    {ok, [Power, Rest], Rs1};

%% 验证角色名字
%% handle(11100, [Name], _Rs) ->
%%     ?DEBUG("10005:~s", [Name]),
%%     case db:get_one("select id from role where name = ~s", [Name]) of
%%         null -> {ok, [0]};
%%         _ -> {ok, [1]}
%%     end;

%% 用帐号登陆
%% handle(11101, [AccountId, Password], Rs) ->
%%     ?DEBUG("account_id:~s, Password:~s", [AccountId, Password]),
%%     case check_password(AccountId, Password) of
%%         true ->
%%             case account_login(AccountId, Rs, false) of
%%                 {ok, Rid, Growth, Rs1} ->
%%                     {ok, [0, Growth, Rid], Rs1};
%%                 {error, no_reg} ->
%%                     ?DEBUG("NO REG", []),
%%                     {ok, [1, 0, 0], Rs#role{account_id = AccountId}};
%%                 {error, _} ->
%%                     ?DEBUG("ERROR", []),
%%                     {ok, [1, 0, 0]}
%%             end;
%%         false ->
%%             ?DEBUG("Password ERROR", []),
%%             {ok, [1, 0, 0]}
%%     end;

%% 注册帐号
%% handle(11102, [Aid, Password], Rs) ->
%%     ?DEBUG("account_id:~w", [Rs#role.account_id]),
%%     ?DEBUG("account_id:~s", [Rs#role.account_id]),
%%     case check_account(Aid) of
%%         true ->
%%             case db:execute("UPDATE `role` SET `aid` = ~s, `password` = ~s WHERE `id` = ~s;", 
%%                     [Aid, Password, Rs#role.id]) of
%%                 1 -> {ok, [0]};
%%                 {error, _} -> {ok, [127]}
%%             end;
%%         false ->
%%             {ok, [1]}
%%     end;

%% 检查帐号是否可注册
handle(11103, [AccountId], _Rs) ->
    case check_account(AccountId) of
        true ->
            {ok, [0]};
        false ->
            {ok, [1]}
    end;

handle(_Cmd, _Data, _RoleState) ->
    {error, bad_request}.

%% === 私有函数 ===

-spec check_password(Rs, AccountId, Password) -> {ok, NewRs} | {error, Reason} when
    Rs :: #role{},
    NewRs :: #role{},
    Reason :: term(),
    AccountId :: binary(),
    Password :: binary().

check_password(Rs, AccountId, Password) ->
    Password1 = get_password_from_ets(AccountId),
    if
        Password =:= <<>> ->
            ?WARN("Password is empty! (AccountId:~s)", [AccountId]),
            {error, empty};
        Password1 =/= false andalso Password1 =:= Password ->
            ?DEBUG("[~s] Rs#role.password =:= Password", [AccountId]),
            {ok, Rs};
        true ->
            Sql = "select count(id) from role where aid = ~s and password = ~s",
            case db:get_one(Sql, [AccountId, Password]) of
                {ok, 1} -> 
                    {ok, Rs#role{account_id = AccountId, password = Password}};
                {ok, C} -> 
                    ?DEBUG("Password Error! AccountId:~w, Count:~w", [AccountId, C]),
                    {error, error};
                {error, null} -> 
                    {error, error};
                {error, Reason} -> 
                    ?DEBUG("Password Error:~w", [Reason]),
                    {error, Reason}
            end
    end.

get_password_from_ets(AccountId) ->
     case lib_role:get_role_pid_from_ets(account_id, AccountId) of
         false -> false;
         {_, Pid} ->
             case catch gen_server:call(Pid, get_state) of
                 {ok, Rs} -> Rs#role.password;
                 _ -> false
             end
     end.

-spec check_account(AccountId) -> true | false when
    AccountId :: binary().

check_account(AccountId) ->
    case db:get_one("select id from role where aid = ~s", 
            [AccountId]) of
        {error, null} -> true;
        _ -> false
    end.

-spec account_login(AccountId, Rs, IsFirst) -> 
    {ok, Rid, Growth, NewRs} | {error, no_reg} | {error, fix_conn} | {error, repeat_login} when 
    AccountId :: binary(),
    Rs :: NewRs,
    NewRs :: #role{},
    IsFirst :: true | false,
    Growth :: integer(),
    Rid :: integer().

account_login(AccountId, Rs, IsFirst) ->
    %% 先检查角色是否仍驻留在内存中
    case lib_role:get_role_pid_from_ets(account_id, AccountId) of
        false ->
            %% 不在内存中，则从数据库加载
            case lib_role:db_init(account_id, AccountId, Rs) of 
                {ok, Rs1} -> 
                    Rs2 = Rs1#role{status = 1},
                    #role{
                        id = Rid
                        ,ip = Ip 
                        ,pid = Pid 
                        ,growth = Growth 
                    } = Rs2,
                    gen_server:cast(myevent, {login, Rid, Pid}),
                    ?LOG({login, Rid, Ip}),
                    case IsFirst of
                        true -> ?LOG({reg, Rid, AccountId, Ip});
                        false -> ok
                    end,
                    {ok, Rid, Growth, Rs2};
                {error, no_reg} ->
                    {error, no_reg};
                {error, Reason} ->
                    {error, Reason}
            end;
        {_From, Pid} when Pid =:= Rs#role.pid ->
            {error, repeat_login};
        {From, Pid} ->
            #role{
                pid_conn = PidConn
                ,pid_sender = PidSerder
                ,socket = Socket
                ,ip = Ip
                ,port = Port
            } = Rs,
            case catch role:fix_conn(Pid, PidConn, PidSerder, Socket, Ip, Port) of
                {'EXIT', Error} ->
                    ?WARN("Error when fix_conn: ~w", [Error]),
                    {error, fix_conn};
                FixRs ->
                    case From of
                        offline -> ets:delete(offline, AccountId);
                        _ -> ok
                    end,
                    Rs#role.pid ! stop,
                    #role{
                        id = FixId
                        ,growth = Growth
                    } = FixRs,
                    {ok, FixId, Growth, Rs}
            end
    end.

%% fix_growth(0) -> 1;
%% fix_growth(I) -> I.
