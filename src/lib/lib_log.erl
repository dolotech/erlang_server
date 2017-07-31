%%----------------------------------------------------
%% log
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(lib_log).
-export([
        online_num/0
        ,retention/0
        ,user_stat/0
    ]
).

-include("common.hrl").

online_num() ->
    try
        Size = ets:info(online, size),
        %% io:format("[online:~w]", [Size]),
        Time = (util:unixtime() div 300) * 300,
        db:execute("INSERT INTO `log_online_num`(`time_stamp`, `day_stamp`, `num`) VALUES (~s, ~s, ~s)", [Time, util:unixtime(today), Size]),
        ok
    catch
        T:X -> ?ERR("online_num[~w:~w]", [T, X])
    end.

%% 统计留存率
retention() ->
    retention1( 2),
    retention1( 3),
    retention1( 4),
    retention1( 5),
    retention1( 6),
    retention1( 7),
    retention1( 8),
    retention1( 9),
    retention1(10),
    retention1(15),
    retention1(30),
    ok.

retention1(Nth) ->
    Sql1 = "SELECT day_stamp FROM `log_login` order by day_stamp asc limit 1",
    PassDay = case db:get_one(Sql1, []) of
        {ok, FirstTime} -> (util:unixtime() - FirstTime) div 86400 + 1;
        {error, null} -> 0;
        {error, Reason} -> 
            ?WARN("retention1: ~w", [Reason]),
            0
    end,
    case PassDay >= Nth of
        true ->
            LoginDay = util:unixtime(yesterday),
            RegDay = LoginDay - (Nth - 1) * 86400,
            {LoginNum, RegNum, Rate} = calc_retention(LoginDay, RegDay),
            {{Y, M, D}, _} = util:mktime({to_date, RegDay}),
            Sql2 = "INSERT INTO `log_retention` (`reg_stamp` , `reg_date`, `reg_num`, `login_num`, `nth_day`, `rate`) VALUES (~s, ~s, ~s, ~s, ~s, ~s);",
            Date = Y * 10000 + M * 100 + D,
            db:execute(Sql2, [RegDay, Date, RegNum, LoginNum, Nth, Rate]);
        false ->
            ok
    end.

calc_retention(LoginDay, RegDay) ->
    try
        util:test1(),
        db:execute("TRUNCATE tmp_reg"),
        db:execute("insert into tmp_reg (id) SELECT id FROM `log_reg` WHERE day_stamp = ~s", [RegDay]),
        db:execute("TRUNCATE tmp_login"),
        db:execute("insert into tmp_login (id) SELECT distinct role_id FROM `log_login` WHERE `day_stamp` = ~s", [LoginDay]),
        Day = (LoginDay - RegDay) div 86400 + 1,
        Query = "select count(*) from tmp_reg join tmp_login on tmp_reg.id = tmp_login.id",
        LoginCount = case db:get_one(Query) of
            {ok, C} -> C;
            {error, _} -> 0
        end,
        {RegCount, _, _} = get_user_stat(RegDay),
        util:test2(<<"CALC RETENTION">>),
        Rate = case RegCount of
            0 -> 0;
            _ -> util:floor(LoginCount / RegCount * 100)
        end,
        ?INFO("reg   day: ~w", [util:mktime({to_date, RegDay})]),
        ?INFO("login day: ~w", [util:mktime({to_date, LoginDay})]),
        ?INFO("[RETENTION ~w] ~w / ~w = ~w", [Day, LoginCount, RegCount, Rate]),
        {LoginCount, RegCount, Rate}
    catch
        T:X -> 
            util:test2(<<"RETENTION">>),
            ?ERR("[RETENTION] ~w:~w", [T, X])
    end.

%%' 读取昨天的用户统计数据（如果不存在则计算并保存）
user_stat() ->
    get_user_stat(util:unixtime(yesterday)).

get_user_stat({Y, M, D}) ->
    DayStamp = util:mktime({{Y, M, D}, {0, 0, 0}}),
    get_user_stat(DayStamp);
get_user_stat(DayStamp0) ->
    {{Y, M, D}, _} = util:mktime({to_date, DayStamp0}),
    DayStamp = case DayStamp0 rem 100 > 0 of
        true -> util:mktime({{Y, M, D}, {0, 0, 0}});
        false -> DayStamp0
    end,
    case db:get_row("SELECT `reg_num`, `active_num`, `online_num` FROM `log_user_stat` WHERE day_stamp = ~s", [DayStamp]) of
        {ok, [RegNum, ActiveNum, OnlineNum]} -> {RegNum, ActiveNum, OnlineNum};
        {error, null} ->
            {ok, RegNum} = calc_reg_num(DayStamp),
            {ok, LoginNum} = calc_login_num(DayStamp),
            OnlineNum = calc_online_top_num(DayStamp),
            ActiveNum = LoginNum - RegNum,
            case DayStamp =< util:unixtime(today) of
                true -> 
                    DayDate = Y * 10000 + M * 100 + D,
                    MonDate = Y * 100 + M,
                    StatSql = "INSERT INTO `log_user_stat`(`day_stamp`, `day_date`, `mon_date`, `reg_num`, `active_num`, `online_num`) VALUES (~s, ~s, ~s, ~s, ~s, ~s);",
                    db:execute(StatSql, [DayStamp, DayDate, MonDate, RegNum, ActiveNum, OnlineNum]);
                false -> ok
            end,
            {RegNum, ActiveNum, OnlineNum};
        {error, _} -> [0, 0, 0]
    end.

%% 计算指定一天的注册量
calc_reg_num(DayStamp) ->
    Sql1 = "SELECT count(*) FROM `log_reg` WHERE `day_stamp` = ~s",
    db:get_one(Sql1, [DayStamp]).

%% 计算指定一天的登陆人数
calc_login_num(DayStamp) ->
    Sql1 = "SELECT count(distinct role_id) FROM `log_login` WHERE day_stamp = ~s",
    db:get_one(Sql1, [DayStamp]).

%% 计算指定一天的在线峰值
calc_online_top_num(DayStamp) ->
    Sql1 = "SELECT max(num) FROM `log_online_num` WHERE day_stamp = ~s",
    case db:get_one(Sql1, [DayStamp]) of
        {ok, undefined} -> 0;
        {ok, C} -> C;
        {error, null} -> 0;
        {error, Reason} -> 
            ?WARN("~w", [Reason]),
            0
    end.
%%.

%% l(0, _, _S, _E) ->
%%     ok;
%% l(Day, 0, S, E) ->
%%     io:format("[Day:~w]", [Day]),
%%     l(Day - 1, 300, S, E);
%% l(Day, Num, S, E) ->
%%     Yesterday = util:unixtime() - 86400,
%%     Time1 = Yesterday - (Day - 1) * 86400,
%%     Rid = util:rand(S, E),
%%     login(Rid, Time1),
%%     io:format("~w", [Rid]),
%%     l(Day, Num - 1, S, E).
%% 
%% reg2() ->
%%     util:test1(),
%%     Yesterday = util:unixtime() - 86400,
%%     Time1 =  Yesterday -  1 * 86400, %% 1天前注册
%%     Time2 =  Yesterday -  2 * 86400, %% 2天前注册
%%     Time3 =  Yesterday -  3 * 86400, %% 3天前注册
%%     Time4 =  Yesterday -  4 * 86400, %% 4天前注册
%%     Time5 =  Yesterday -  5 * 86400, %% 5天前注册
%%     Time6 =  Yesterday -  6 * 86400, %% 6天前注册
%%     Time7 =  Yesterday -  7 * 86400, %% 7天前注册
%%     Time10 = Yesterday - 10 * 86400, %% 10天前注册
%%     Time15 = Yesterday - 15 * 86400, %% 15天前注册
%%     util:for(    1, 300, fun(Rid) -> login1(Rid, Time1 ) end), ?INFO("1"),
%%     util:for( 301,  500, fun(Rid) -> login1(Rid, Time2 ) end), ?INFO("2"),
%%     util:for( 501,  600, fun(Rid) -> login1(Rid, Time3 ) end), ?INFO("3"),
%%     util:for( 601,  700, fun(Rid) -> login1(Rid, Time4 ) end), ?INFO("4"),
%%     util:for( 701,  800, fun(Rid) -> login1(Rid, Time5 ) end), ?INFO("5"),
%%     util:for( 801,  900, fun(Rid) -> login1(Rid, Time6 ) end), ?INFO("6"),
%%     util:for( 901, 1000, fun(Rid) -> login1(Rid, Time7 ) end), ?INFO("7"),
%%     util:for(1001, 1100, fun(Rid) -> login1(Rid, Time10) end), ?INFO("8"),
%%     util:for(1101, 1200, fun(Rid) -> login1(Rid, Time15) end), ?INFO("9"),
%%     l(1 , 200,    1,  300), ?INFO("10"),
%%     l(2 , 200,  301,  500), ?INFO("11"),
%%     l(3 , 200,  501,  600), ?INFO("12"),
%%     l(4 , 200,  601,  700), ?INFO("13"),
%%     l(5 , 200,  701,  800), ?INFO("14"),
%%     l(6 , 200,  801,  900), ?INFO("15"),
%%     l(7 , 200,  901, 1000), ?INFO("16"),
%%     l(10, 200, 1001, 1100), ?INFO("17"),
%%     l(15, 200, 1101, 1200), util:test2("END"),
%%     ok.
%% 
%% regs() ->
%%     util:test1(),
%%     Now = util:unixtime() - 86400,
%%     Time1  = Now -  1 * 86400,
%%     Time2  = Now -  2 * 86400,
%%     Time3  = Now -  3 * 86400,
%%     Time4  = Now -  4 * 86400,
%%     Time5  = Now -  5 * 86400,
%%     Time6  = Now -  6 * 86400,
%%     Time7  = Now -  7 * 86400,
%%     Time10 = Now - 10 * 86400,
%%     Time15 = Now - 15 * 86400,
%%     spawn(fun() -> util:for(    1,  3000, fun(Rid) -> login1(Rid, Time1 ) end) end),
%%     spawn(fun() -> util:for( 3001,  5000, fun(Rid) -> login1(Rid, Time2 ) end) end),
%%     spawn(fun() -> util:for( 5001,  6000, fun(Rid) -> login1(Rid, Time3 ) end) end),
%%     spawn(fun() -> util:for( 6001,  7000, fun(Rid) -> login1(Rid, Time4 ) end) end),
%%     spawn(fun() -> util:for( 7001,  8000, fun(Rid) -> login1(Rid, Time5 ) end) end),
%%     spawn(fun() -> util:for( 8001,  9000, fun(Rid) -> login1(Rid, Time6 ) end) end),
%%     spawn(fun() -> util:for( 9001, 10000, fun(Rid) -> login1(Rid, Time7 ) end) end),
%%     spawn(fun() -> util:for(10001, 11000, fun(Rid) -> login1(Rid, Time10) end) end),
%%     spawn(fun() -> util:for(11001, 12000, fun(Rid) -> login1(Rid, Time15) end) end),
%% 
%%     spawn(fun() -> l(1 , 300,     1,  3000) end),
%%     spawn(fun() -> l(2 , 300,  3001,  5000) end),
%%     spawn(fun() -> l(3 , 300,  5001,  6000) end),
%%     spawn(fun() -> l(4 , 300,  6001,  7000) end),
%%     spawn(fun() -> l(5 , 300,  7001,  8000) end),
%%     spawn(fun() -> l(6 , 300,  8001,  9000) end),
%%     spawn(fun() -> l(7 , 300,  9001, 10000) end),
%%     spawn(fun() -> l(10, 300, 10001, 11000) end),
%%     spawn(fun() -> l(15, 300, 11001, 12000) end),
%%     util:test2("END"),
%%     ok.
%% 
%% login(Rid, Time) ->
%%     {Date, _} = util:mktime({to_date, Time}),
%%     Time0 = util:mktime({Date, {0, 0, 0}}),
%%     catch db:execute("INSERT INTO `log_login`(`role_id`, `event`, `ip`, `login_day`, `login_time`) VALUES (~s, 0, ~s, ~s, ~s)", [Rid, <<"127.0.0.1">>, Time0, Time]).
%% 
%% login1(Rid, Time) ->
%%     {Date, _} = util:mktime({to_date, Time}),
%%     Time0 = util:mktime({Date, {0, 0, 0}}),
%%     catch db:execute("INSERT INTO `log_login`(`role_id`, `event`, `ip`, `first`, `login_day`, `login_time`) VALUES (~s, 0, ~s, 1, ~s, ~s)", [Rid, <<"127.0.0.1">>, Time0, Time]).

