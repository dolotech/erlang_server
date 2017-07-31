%%----------------------------------------------------
%% include
%% 
%% @author rolong@vip.qq.com
%%----------------------------------------------------

-include("define.hrl").
-include("role.hrl").
-include("online.hrl").
-include("item.hrl").
 
%% MySQL result record:
-record(mysql_result, {fieldinfo=[], rows=[], affectedrows=0, error=""}).

%% 战区信息数据结构
-record(node, {id, name, host, port, room_num = 0, role_num = 0}).
