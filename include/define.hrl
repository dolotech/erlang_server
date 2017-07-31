%%----------------------------------------------------
%% 宏定义
%% 
%% @author rolong@vip.qq.com
%%----------------------------------------------------

-define(TAB_EQU , 5).  %% 装备标签
-define(TAB_PROP, 2).  %% 道具标签
-define(TAB_MAT , 1).  %% 材料标签 (material)

-define(DETS_DIR, "dets").
-define(DB, mysql_conn_poll).
-define(FL_POLICY_REQ, <<"<policy-file-request/>\0">>).
-define(ESC_QUOTES(Bin_), binary:replace(Bin_, [<<92>>, <<34>>], <<92>>, [global,{insert_replaced,1}])).
-define(ESC_SINGLE_QUOTES(Bin_), binary:replace(Bin_, [<<92>>, <<39>>], <<92>>, [global,{insert_replaced,1}])).

-define(LOG(Msg), mylogger:log(Msg)).
-define(LOG_GOLD(R1, R2, Type), mylogger:log(
        {gold, R1#role.id, Type, R2#role.gold - R1#role.gold, R2#role.gold}
    )).
-define(LOG_CARD(R1, R2, Type), mylogger:log(
        {card, R1#role.id, Type, R2#role.card - R1#role.card, R2#role.card}
    )).


-define(INFO(Msg), util:info(Msg, [], ?MODULE, ?LINE)).
%% -define(DEBUG(Msg), mylogger:notify(debug, Msg, [], ?MODULE, ?LINE)).
-define(DEBUG(Msg), util:debug(Msg, [], ?MODULE, ?LINE)).
-define(ERR(Msg), mylogger:notify(error, Msg, [], ?MODULE, ?LINE)).
-define(INFO(F, A), util:info(F, A, ?MODULE, ?LINE)).
%% -define(DEBUG(F, A), mylogger:notify(debug, F, A, ?MODULE, ?LINE)).
-define(DEBUG(F, A), util:debug(F, A, ?MODULE, ?LINE)).
-define(WARN(F, A), mylogger:notify(warning, F, A, ?MODULE, ?LINE)).
-define(WARNING(F, A), mylogger:notify(warning, F, A, ?MODULE, ?LINE)).
-define(ERR(F, A), mylogger:notify(error, F, A, ?MODULE, ?LINE)).
-define(REC_SET(R, T, T2, K, V), (R#T.T2)#T2{K=V}).
-define(REC_SET(R, T, T2, Kvs), lists:foldl(fun({K, V}) -> ?REC_SET(R, T, T2, K, V) end, R, Kvs).
-define(REC_GET(R, T, T2, K), (R#T.T2)#T2.K).

-define(EQU_POS, [97, 98, 99, 100, 101, 102, 103, 104]).
-define(EQU_TYPE, [1, 4, 5, 6, 11, 12, 15, 20]).

-define(GAME_MODES, [1, 2, 101, 102, 103]).

-define(ETS_MAP_POS, map_pos).
-define(T(Text), Text).

-define(MYDAOJU_MAX, 2).

%% -define(OFFLINE_CACHE_TIME, 600000).
-define(OFFLINE_CACHE_TIME, 5).

-define(BIT_MAX, 16#ffffffff).

-define(BIT_BAN_CHAT, 2#1).

-define(MIN_EQU_ID, 100000).

%% 定义事件
-define(EVENT_REQUEST_LEV, 1).
-define(EVENT_RESPOND_LEV, 2).
-define(EVENT_ADD_FANS, 3).
-define(EVENT_DEL_FANS, 4).
-define(EVENT_SET_GUILD, 5).
-define(EVENT_TASK, 6).
-define(EVENT_SHOPPING, 7).
-define(EVENT_ADD_ITEM, 8).
-define(EVENT_ADD_GOLD, 9).
-define(EVENT_ADD_CARD, 10).
-define(EVENT_INVOKE, 11).
-define(EVENT_SEND_CODE, 12).
-define(EVENT_BAN_CHAT, 13).

%% 1,攻击
%% 2,血量
%% 3,防御
%% 4,穿刺
%% 5,命中
%% 6,闪避
%% 7,暴击
%% 8,暴强
%% 9,免暴
%% 10,韧性
-define(atk      ,1).
-define(hp       ,2).
-define(def      ,3).
-define(pun      ,4).
-define(hit      ,5).
-define(dod      ,6).
-define(crit     ,7).
-define(crit_num ,8).
-define(crit_anti ,9).
-define(tou ,10).
