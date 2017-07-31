%%----------------------------------------------------
%% ROBOT
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-define(AID_PREFIX, <<"robot">>).

-record(robot, {
        id
        ,sex = 0
        ,aid %% account id
        ,name
        ,pid
        ,pid_sender
        ,socket
        ,tmp_package = <<>>
        ,gold = 0
        ,diamond = 0
        ,essence = 0
        ,handle_name = robot_handle

		,bag_equ_max    = 14    %% 装备背包格子数，默认2行14个
		,bag_prop_max   = 14    %% 道具背包格子数，默认2行14个
		,bag_mat_max    = 14    %% 材料背包格子数，默认2行14个

        ,power
        ,tollgateid
        ,picture
    }
).
