%%----------------------------------------------------
%% @author rolong@vip.qq.com
%%----------------------------------------------------

-include("define.hrl").

%% some log defines

-define(CRON_FILE, "crontab.config"). %% filename of the config
-define(CHECK_FILE_INTERVAL, 60 * 1000).
-define(CHECK_CRON_INTERVAL, 60 * 1000).

-define(CRON_ANY,   1). % "*"
-define(CRON_NUM,   2). % 2
-define(CRON_RANGE, 4). % 2-3
-define(CRON_LIST,  8). % "2,3-6"

-record(cron_field, {
     type = ?CRON_ANY
    ,value 
}).

-record(cron_entry, {
     m   % minute
    ,h   % hour
    ,dom % day of month
    ,mon % month
    ,dow % day of week
    ,mfa % the mfa
}).
