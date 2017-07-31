%%----------------------------------------------------
%%
%% 
%% @author hanssonlan@126.com
%%----------------------------------------------------
-module(crontab_lib).
-include("crontab.hrl").

-export([parse/1]).

parse(File) ->
    case file:consult(File) of
        {error, enoent} = Error ->
            ?INFO("crontab file ~p not exist", [File]),
            Error;
        {error, R} = Error ->
            ?INFO("crontab file error: ~p", [file:format_error(R)]),
            Error;
        {ok, CronTab} ->
            parse_entrys(CronTab)
    end.

%%-----------------------------------------------------------------------
%%
%% internal API
%%
%%-----------------------------------------------------------------------

%% parse all the entrys
parse_entrys(CronTab) ->
    Entrys =
    lists:foldl(
        fun(Entry, Acc) ->
                case catch parse_entry(Entry) of
                    {ok, CronEntry} ->
                        [CronEntry | Acc];
                    {error, R} ->
                        ?INFO("the line: ~p error: ~p", [Entry, R]),
                        Acc
                end
        end,
        [],
        CronTab),
    {ok, Entrys}.

%% parse the single entry
parse_entry({{M, H, Dom, Mon, Dow}, {Mod, F, A} = MFA}) when is_atom(Mod), is_atom(F), is_list(A) ->
    Cron =
    #cron_entry{
        m   = parse_field(M  , 0, 59, {error, emin }),
        h   = parse_field(H  , 0, 23, {error, ehour}),
        dom = parse_field(Dom, 1, 31, {error, edom }),
        mon = parse_field(Mon, 1, 12, {error, emon }),
        dow = parse_field(Dow, 0,  7, {error, edow }),
        mfa = MFA
    },
    {ok, Cron}; 
parse_entry(_) ->
    {error, eformat}.


%% parset the fileld
parse_field(F, Min, Max, Error) ->
    try parse_field(F, Min, Max) of
        {?CRON_ANY} ->
            #cron_field{type = ?CRON_RANGE, value = {Min, Max, 1}};
        {?CRON_NUM, N} ->
            #cron_field{type = ?CRON_NUM, value = N};
        {?CRON_RANGE, {_First, _Last, _Step} = Range} ->
            #cron_field{type = ?CRON_RANGE, value = Range};
        {?CRON_LIST, List} ->
            #cron_field{type = ?CRON_LIST, value = List};
        _Err ->
            ?INFO("== Err: ~p", [_Err]),
            throw(Error)
    catch _Type:_Why ->
        ?INFO("== Type: ~p Why: ~p", [_Type, _Why]),
        throw(Error)
    end.


parse_field("*", _Min, _Max) ->
    {?CRON_ANY};
parse_field(F, Min, Max) when F >= Min, F =< Max ->
    {?CRON_NUM, F};
parse_field(F = [_|_], Min, Max) when is_list(F) ->
    case string:tokens(F, ",") of
        [Single] -> % is range
            case parse_range(Single) of
                {First, Last, _Step} = Range when First >= Min, Last =< Max ->
                    {?CRON_RANGE, Range}
            end;
        [_|_] = Multi -> % is list
            {?CRON_LIST, [parse_field(E, Min, Max, elist) || E <- Multi]}
    end.
     

%% parse the range string: "2-5/2", "2-5"
parse_range(Str) ->
    {RangeStr, Step} = 
    case string:tokens(Str, "/") of
        [Range] ->
            {Range, 1};
        [Range, StepStr] ->
            {Range, list_to_integer(StepStr)}
    end,
    [First, Last] = string:tokens(RangeStr, "-"),
    {list_to_integer(First), list_to_integer(Last), Step}.

