%%----------------------------------------------------
%%
%% 
%% @author hanssonlan@126.com
%%----------------------------------------------------
-module(crontab).
-behaviour(gen_server).
-include("crontab.hrl").

-export([start_link/0]).
-export([init/1, 
        handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).


-record(state, {
     file = ""   % file name
    ,mtime = 0   % last modify time
    ,entrys = [] % the cron tasks
    ,file_timer  % the check file last modified timer
    ,cron_timer  % the check cron task timer
}).

-define(SERVER, ?MODULE).
        
%% @doc start the cron server
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
    %% gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%% gen_server callbacks
init(_Args) ->
    process_flag(trap_exit, true),
    ?INFO("~p start_link", [?SERVER]),
    case crontab_lib:parse(?CRON_FILE) of
        {ok, Entrys} ->
            ?INFO("parse the crontab success!", []),
            State = #state{
                file = ?CRON_FILE,
                mtime = filelib:last_modified(?CRON_FILE),
                entrys = Entrys,
                file_timer = check_file_timer(),
                cron_timer = check_cron_timer()
            },
            {ok, State};
        Error ->
            ?INFO("error :~p", [Error]),
            Error
    end.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, _Ref, check_file}, State = #state{file = File, mtime = MTime}) ->
    %% ?INFO("check the file :~p", [File]),
    State2 = State#state{
        file_timer = check_file_timer()
    },
    MTimeNew = filelib:last_modified(File),
    case  MTimeNew > MTime of
        true -> % reload crontab
            case crontab_lib:parse(File) of
                {ok, Entrys} ->
                    State3 = State2#state{
                        file = File,
                        mtime = MTimeNew,
                        entrys = Entrys
                    },
                    {noreply, State3};
                _Error ->
                    ?INFO("the crontab file ~s format error:~p", [File, _Error]),
                    {noreply, State2}
            end;
        false ->
            {noreply, State2}
    end;

handle_info({timeout, _Ref, check_cron}, State = #state{entrys = Entrys}) ->
    %% ?INFO("check the cron", []),
    State2 = State#state{
        file_timer = check_cron_timer()
    },
    check_entrys(Entrys),
    {noreply, State2};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
    
%%-----------------------------------------------------------------------------
%%
%% internal API
%%
%%-----------------------------------------------------------------------------

%% start the check file timer
check_file_timer() ->
    erlang:start_timer(?CHECK_FILE_INTERVAL, self(), check_file).

%% start the cron tasks timer
check_cron_timer() ->
    erlang:start_timer(?CHECK_CRON_INTERVAL, self(), check_cron).

%% check the cron entrys
check_entrys(Entrys) ->
    Now = {Date, _Time} = erlang:localtime(), 
    Week = calendar:day_of_the_week(Date),
    lists:foreach(
        fun(Entry) ->
                case can_run(Entry, Now, Week) of
                    true ->
                        %% ?INFO("run this task:~p", [Entry]),
                        run_task(Entry#cron_entry.mfa);
                    false ->
                        %% ?INFO("can't run this task:~p", [Entry]),
                        ok
                end
        end,
        Entrys).

can_run(Entry, {{_, CurMon, CurDay}, {CurH, CurM, _}}, Week) ->
    #cron_entry{
        m = M,
        h = H,
        dom = Dom,
        mon = Mon,
        dow = Dow
    } = Entry,
    field_ok(M, CurM) andalso
    field_ok(H, CurH) andalso
    field_ok(Dom, CurDay) andalso
    field_ok(Dow, Week) andalso
    field_ok(Mon, CurMon). 

%% check if the field is ok
field_ok(#cron_field{type = ?CRON_NUM, value = Val}, Cur) ->
    Val =:= Cur;
field_ok(#cron_field{type = ?CRON_RANGE, value = {First, Last, Step}}, Cur) ->
    range_ok(Cur, First, Last, Step);
field_ok(#cron_field{type = ?CRON_LIST, value = List}, Cur) ->
    lists:any(
        fun(FInList) ->
                field_ok(FInList, Cur)
        end,
        List).

%% check if the value in the range
range_ok(Val, First, Last, Step) ->
    range_ok1(Val, First, Last, Step).

range_ok1(Val, Val, _Last, _Step) ->
    true;
range_ok1(_Val, Cur, Last, _Step) when Cur >= Last ->
    false;
range_ok1(Val, Cur, Last, Step) ->
    range_ok1(Val, Cur + Step, Last, Step).


%% run the task
run_task({M, F, A} = Task) ->
    %% ?INFO("run the cron task:{~p, ~p, ~p}", [M, F, A]),
    proc_lib:spawn(
        fun() ->
            case catch apply(M, F, A) of
                {'EXIT', R} ->
                    ?INFO("cron task ~p error: ~p", [Task, R]),
                    ok;
                _ ->
                    ok
            end
        end
    ).
