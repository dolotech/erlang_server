%%----------------------------------------------------
%% Robot handle
%%
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(robot_handle_p).
-export([handle/3]).

-include("common.hrl").
-include("robot.hrl").

%%' 10协议_登陆认证

handle(11001, [LoginState, Growth, Rid], R) ->
    ?INFO("11001:~w", [[LoginState, Growth, Rid]]),
    case LoginState of
        0 ->
            case Growth of
                0 ->
                    %% 创建角色
                    Sex = util:rand(1, 2),
                    SexS = case Sex of
                        1 -> <<"男">>;
                        2 -> <<"女">>;
                        _ -> <<"未知">>
                    end,
                    Name = list_to_binary([?AID_PREFIX, integer_to_list(R#robot.aid)]),
                    ?INFO("Reg:~s - ~s", [SexS, Name]),
                    R#robot.pid_sender ! {cmd, 11002, [Name, Sex]},
                    R#robot{sex = Sex, name = Name};
                _ ->
                    %% Name = list_to_binary([?AID_PREFIX, integer_to_list(R#robot.aid)]),
                    %% 正常登陆
                    io:format("+"),
                    R#robot.pid_sender ! {cmd, 11003, []},
                    %% R#robot.pid_sender ! {cmd, 11005, [<<"joe">>]},
                    %% R#robot.pid_sender ! {cmd, 11007, []},
                    %% R#robot.pid_sender ! {cmd, 13002, [3,3456,5]},

					%% R#robot.pid_sender ! {cmd, 13001, [1]},
                    %% R#robot.pid_sender ! {cmd, 13002, [1,11012,1]},
                    %% R#robot.pid_sender ! {cmd, 13006, [3]},
                    %% R#robot.pid_sender ! {cmd, 13010, [1,13]},
                    %% R#robot.pid_sender ! {cmd, 13012, [101001]},
                    %% R#robot.pid_sender ! {cmd, 13022, [1]},
                    %% R#robot.pid_sender ! {cmd, 13021, [4, 2]},

                    %% R#robot.pid_sender ! {cmd, 13025, []},
                    R#robot.pid_sender ! {cmd, 13027, [1, 1, 2]},
                    %% R#robot.pid_sender ! {cmd, 13028, [1, 1]},

                    %% R#robot.pid_sender ! {cmd, 14002, []},
                    %% R#robot.pid_sender ! {cmd, 14005, [1,2,3]},
                    %% R#robot.pid_sender ! {cmd, 14019, [1,0]},
                    %% R#robot.pid_sender ! {cmd, 14010, [2]},
                    %% R#robot.pid_sender ! {cmd, 14020, [1]},
					%% R#robot.pid_sender ! {cmd, 23001, [Name, 1]},
					%% R#robot.pid_sender ! {cmd, 23015, []},
					%% R#robot.pid_sender ! {cmd, 23028, []},
					%% R#robot.pid_sender ! {cmd, 23029, [1]},
					R#robot.pid_sender ! {cmd, 24006, []},

                    R#robot{id = Rid, pid = self()}
            end;
        1 ->
            io:format("login failure! ~w~n", [[LoginState, Growth, Rid]]),
            R
    end;
%%.

handle(11002, [0], _R) ->
    ?INFO("Create ok!"),
    %% 创建角色成功
    ok;

handle(11002, [1], _R) ->
    ?INFO("Please rename!"),
    ok;

handle(11003, [Diamond, Gold, Tollgateid, Tired], R) ->
    ?INFO("===*=== Diamond:~w, Gold:~w Tollgateid:~w, Tired:~w ===*===", [Diamond, Gold, Tollgateid, Tired]),
    R#robot{gold = Gold, diamond = Diamond};

handle(11007,[Power, Rest], _R) ->
    ?INFO("=== Power:~w, Rest:~w ===",[Power, Rest]);
    % R#robot{};

% handle(11005, [Name], _R) ->
%     ?INFO("Name is:~p", [Name]),
%     ok;
handle(13001, [1, Data1, Data2], _R) ->
	?INFO("===Data1:~w, Data2:~w", [Data1, Data2]);

handle(13010, [Code, EquID], _R) ->
    ?INFO("===Code:~w,EquID:~w ===", [Code, EquID]);

handle(13012, [Code],_R) ->
    if
        Code == 0 ->
            ?INFO("good job");
        Code == 1 ->
            ?INFO("combining failed");
        Code == 2 ->
            ?INFO("not more data");
        true ->
            ?ERR("Code:~w ", [Code])
    end;

handle(Cmd, Data, _R) ->
    io:format("Recv [Cmd:~w Data:~w]~n", [Cmd, Data]).


%%% vim: set filetype=erlang foldmarker=%%',%%. foldmethod=marker:
