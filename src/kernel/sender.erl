%%----------------------------------------------------
%% 发包API
%% 
%% @author Rolong<erlang6@qq.com>
%%----------------------------------------------------
-module(sender).
-export(
    [
        pack_send/3
        ,pack_cast/4
        ,pack_cast/3
        ,send_error/1
        ,send_error/2
        ,send_code/1
        ,send_code/2
        ,cast/3
        ,cast_event/3
    ]
).

-include("common.hrl").
-include("offline.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%% 广播
send_error(Code) ->
    pack_cast(world, 10001, [1, Code]).

send_error(Id, Code) ->
    pack_send(Id, 10001, [1, Code]).

send_code(Code) ->
    pack_cast(world, 10001, [2, Code]).

send_code(Id, Code) ->
    pack_send(Id, 10001, [2, Code]).

%% 打包并发送消息(通过指定的socket发包进程转发)
pack_send(PidSender, Cmd, Data) when is_pid(PidSender) ->
    case pt_pack:p(Cmd, Data) of
        {ok, Bin} -> 
            %% ?INFO("SEND [Cmd:~w, Bin:~w]", [Cmd, Bin]),
            PidSender ! {data, Bin};
        Other -> Other
    end;

%% 打包并发送消息(直接socket发送)
pack_send(Socket, Cmd, Data) when is_port(Socket) ->
    case pt_pack:p(Cmd, Data) of
        {ok, Bin} -> gen_tcp:send(Socket, Bin);
        Other ->
            Other
    end;

%% 打包并发送消息(角色ID)
pack_send(Rid, Cmd, Data) when is_integer(Rid) ->
    case ets:lookup(online, Rid) of
        [Role] -> pack_send(Role#online.pid_sender, Cmd, Data);
        _ -> skip
    end.

pack_cast(Type, Id, Cmd, Data) ->
    case pt_pack:p(Cmd, Data) of
        {ok, Bin} ->  
            broadcast(Type, Id, Bin);
        Other -> 
            ?ERR("pack error [room_id:~w, cmd:~w, Data:~w]", [Id, Cmd, Data]),
            Other
    end.

pack_cast(world, Cmd, Data) ->
    case pt_pack:p(Cmd, Data) of
        {ok, Bin} ->  
            broadcast(world, 0, Bin);
        Other -> 
            ?ERR("[PACK ERROR] world, cmd:~w, Data:~w]", [Cmd, Data]),
            Other
    end;

pack_cast(Senders, Cmd, Data) ->
    case pt_pack:p(Cmd, Data) of
        {ok, Bin} ->
            send(Senders, Bin);
        _Any -> 
            ?ERR("send error :~w", [_Any]),
            ok
    end.

send([], _Bin) -> ok;
send([Sender | T], Bin) ->
    Sender ! {data, Bin},
    send(T, Bin).

%% 广播数据包(手动打包)
cast(Senders, Cmd, Data) ->
    Len = byte_size(Data),
    Bin = <<Len:16, Cmd:16, Data/binary>>,
    send(Senders, Bin).

broadcast(world, _Id, Data) ->
    case ets:match(online, #online{pid_sender='$1', _='_'}) of
        {error, Reason} -> ?ERR("Error When Read Online Data:~w", [Reason]);
        L -> do_broadcast(L, Data)
    end.

%% 对列表中的所有socket进行广播
do_broadcast([], _Data) -> ok;
do_broadcast([[P] | T], Data) ->
    case is_pid(P) of
        true -> P ! {data, Data};
        false -> 
            ?WARNING("do_broadcast:~w", [P]),
            ok
    end,
    do_broadcast(T, Data).

cast_event(world, Cmd, Data) ->
    cast_event1(world, Cmd, Data),
    cast_event2(world, Cmd, Data),
    ok.

cast_event1(world, Cmd, Data) ->
    case ets:match(online, #online{pid='$1', _='_'}) of
        {error, Reason} -> ?WARNING("ets:match:~w", [Reason]);
        L -> do_cast_event(L, Cmd, Data)
    end.

cast_event2(world, Cmd, Data) ->
    case ets:match(offline, #offline{pid='$1', _='_'}) of
        {error, Reason} -> ?WARNING("ets:match:~w", [Reason]);
        L -> do_cast_event(L, Cmd, Data)
    end.

do_cast_event([[P] | T], Cmd, Data) ->
    case is_pid(P) of
        true -> P ! {handle_event, Cmd, Data};
        false -> 
            ?WARNING("do_cast_event:~w", [P]),
            ok
    end,
    do_cast_event(T, Cmd, Data);
do_cast_event([], _, _) -> ok.
