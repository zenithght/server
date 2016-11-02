%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc 玩家属性
%%%
%%% Created : 22. 七月 2016 下午5:07
%%%-------------------------------------------------------------------
-module(chat_handler).

-include("pub.hrl").
-include("player_behaviour.hrl").

-export([handle_info/3]).

load_data(State) -> State.


online(State) -> State.


online_send_data(State) -> State.


save_data(State) -> State.


terminate(State) ->
    save_data(State).


handler_frame(State, _Event) -> State.


handler_msg(State, _FromPid, _FromModule, {abcast, ScrollId}) ->
    case scroll_srv:select_id(ScrollId) of
        [] -> ok;
        Content -> ?tcp_send(State, chat_sproto:encode(?PROTO_PUSH_NOTICE, [Content]))
    end,
    State;

handler_msg(State, _FromPid, _FromModule, {?private_chat, ChatUid, Times, MsgType, Msg}) ->
    {ok, [Nick, Icon]} = redis_attr:get(ChatUid, [?nick, ?icon]),
    ?tcp_send(State, friend_sproto:encode(?PROTO_FRIEND_RECV_PRIVATE_CHAT, [[Times, ChatUid, Nick, Icon, MsgType, Msg]])),
    
    case redis_online:is_online(ChatUid) of
        {ok, Node, Pid} ->
            node_srv:send_to_node(ChatUid, Pid, Node, ?remote_mod_msg(?chat_handler, {?private_chat, ok}));
        {ok, Pid} ->
            ?send_mod_msg(Pid, ?MODULE, {?private_chat, ok});
        _ ->
            ok
    end,
    State;

handler_msg(State, _FromPid, _FromModule, {?private_chat, ok}) ->
    ?tcp_send(State, friend_sproto:encode(?PROTO_FRIEND_PRIVATE_CHAT, 1));

handler_msg(State, _FromPid, _FromModule, _Msg) -> State.


handle_info(State, ?PROTO_FRIEND_PRIVATE_CHAT, [Uid, Type, Msg]) ->
    assert_can:natural_num(Uid),
    case friend_def:private_chat_type(Type) of
        <<>> -> ?return_err(?ERR_ARG_ERROR);
        _ -> ok
    end,
    
    case load_friend:is_my_friend(State, Uid) of
        false -> ?return_err(?ERR_FPI_NOT_FOLLOW);
        _ -> ok
    end,
    
    MsgByte = byte_size(Msg),
    if
        MsgByte >= 100 -> ?return_err(?ERR_FRI_MSG_TOO_LONG);
        true -> ok
    end,
    
    Now = erl_time:now(),
    case redis_online:is_online(Uid) of
        {ok, Pid} ->
            ?send_mod_msg(Pid, ?chat_handler, {?private_chat, State#player_state.uid, Now, Type, Msg});
        {ok, Node, Pid} ->
            node_srv:send_to_node(Uid, Pid, Node, ?remote_mod_msg(?chat_handler, {?private_chat, State#player_state.uid, Now, Type, Msg}));
        false ->
            redis_chat_msg:set(Uid, State#player_state.uid, Now, Type, Msg),
            ?tcp_send(State, friend_sproto:encode(?PROTO_FRIEND_PRIVATE_CHAT, 1))
    end,
    State;

handle_info(State, _Cmd, _RawData) ->
    ?LOG("handle_info no match ProtoId:~p~n Data:~p~n state:~p~n", [_Cmd, _RawData, State]),
    State.