%%----------------------------------------------------
%% 副本
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------

-record(fb, {
          id %% 副本ID
          ,map_id
          ,map_pos1
          ,map_pos2
          ,mypos1
          ,mypos2
          ,refresh_time
          ,free_times
          ,monsters %% [#fbm]
         }).

%% fb monster
-record(fbm, {
          pos %% pos即为地图位置
          ,type %% elite | boss | bigboss
          ,gate_id %% 关卡ID
          ,status %% 0为已被击败，1为可战斗
          ,revival_time %% 复活时间
         }).

