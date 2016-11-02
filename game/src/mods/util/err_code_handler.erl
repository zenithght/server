%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%%
%%% Created : 14. 七月 2016 下午2:31
%%%-------------------------------------------------------------------
-module(err_code_handler).

-include("pub.hrl").

-export([
    err_code/3,
    err_code/5,
    handle_info/3]).

err_code(State, _Msg, {throw, ErrCode}) ->
    {ok, {_Ip, _Port}} = inet:peername(State#player_state.socket),
    ?LOG("err_code IP:~w...ERR_CODE:~w...MSG:~w~n", [_Ip, ErrCode, [_Msg, State]]),
    case get(?proto_id) of
        ?undefined ->
            ?tcp_send(State, [0, 0, ErrCode]);
        {ModId, ProtoId} ->
            ?tcp_send(State, [ModId, ProtoId, [<<"err_code">>, ErrCode]])
    end.


err_code(State, Error, ModId, ProtoId, Data) ->
    case Error of
        {throw, ErrCode} ->
            {ok, {_Ip, _Port}} = inet:peername(State#player_state.socket),
            ?LOG("err_code ModId:~w...protoId:~w...Data:~w...ERR_CODE:~w...State:~w~n", [[_Ip, ModId], ProtoId, Data, ErrCode, State]),
            
            ?tcp_send(State, [ModId, ProtoId, [<<"err_code">>, ErrCode]]);
        _ ->
            ?ERROR("crash:~p~n", [[ModId, ProtoId, Data, Error]]),
            ?tcp_send(State, [0, 0, 0])
    end.



%% 进入游戏
handle_info(State, ?PROTO_SYS_TICK, []) ->
    State;


handle_info(State, _Cmd, _RawData) ->
    ?LOG("handle_info no match ProtoId:~p~n Data:~p~n state:~p~n", [_Cmd, _RawData, State]),
    State.