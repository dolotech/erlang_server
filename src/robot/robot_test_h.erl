%%----------------------------------------------------
%% Robot handle 
%%
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(robot_test_h).
-export([handle/3]).

-include("common.hrl").
-include("robot.hrl").

%%' 10协议_登陆认证

handle(11001, [LoginState, Growth, Rid], R) ->
    %% ?INFO("11001:~w", [[LoginState, Growth, Rid]]),
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
                    %% 正常登陆
                    io:format("+"),
                    R#robot.pid_sender ! {cmd, 11003, []},
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

handle(11003, [Diamond, Gold, Tollgateid, Tired], R) ->
    ?INFO("===*=== Diamond:~w, Gold:~w Tollgateid:~w, Tired:~w ===*===", [Diamond, Gold, Tollgateid, Tired]),
    R#robot{gold = Gold, diamond = Diamond};

handle(11002, [1], _R) ->
    ?INFO("Please rename!"),
    ok;

handle(Cmd, Data, _R) ->
    io:format("Recv [Cmd:~w Data:~w]~n", [Cmd, Data]).

%%% vim: set filetype=erlang foldmarker=%%',%%. foldmethod=marker:
