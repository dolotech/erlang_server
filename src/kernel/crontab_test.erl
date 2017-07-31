%%----------------------------------------------------
%%
%% 
%% @author hanssonlan@126.com
%%----------------------------------------------------
-module(crontab_test).
-compile(export_all).
-include("crontab.hrl").

parse() ->
    parse(?CRON_FILE).

parse(File) ->
    case crontab_lib:parse(File) of
        {ok, I} -> io:format("success!~n~p", [I]);
        _ -> ok
    end.
