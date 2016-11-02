-module(t_player_server).

-behaviour(websocket_client_handler).

-include("pub.hrl").

-export([
    start_link/0,
    init/2,
    websocket_handle/3,
    websocket_info/3,
    websocket_terminate/3
]).

-define(SERVER_WS_IP, "ws://127.0.0.1:8089/").


start_link() ->
    crypto:start(),
    inets:start(),
    ssl:start(),
    websocket_client:start_link(?SERVER_WS_IP, ?MODULE, []).

init([], _ConnState) ->
    put(validity, 0),
%%    websocket_client:cast(self(), {text, <<"message 1">>}),
    {ok, 2}.

websocket_handle({pong, _}, _ConnState, State) ->
    ?LOG("websocket_handle/3 arg:~p~n", [_ConnState]),
    {ok, State};

websocket_handle({text, _Msg}, _ConnState, 5) ->
    ?LOG("Received msg ~p~n", [_Msg]),
    {close, <<>>, "done"};

websocket_handle({text, Msg}, _ConnState, State) ->
    ?LOG("Received msg ~p~n", [?proto_decode(Msg)]),
    DecodeData = ?proto_decode(Msg),
    case get(validity) of
        0 ->
            put(?pack_random, DecodeData),
            put(validity, 1),
            Data = t_login_handler:send(?PROTO_LOGIN, {}),
            ?LOG("111:~p~n", [Data]),
            {reply, {text, ?encode([ws_uniform() | Data])}, State};
%%            {ok, State};
        1 ->
            ?LOG("222:~n"),
            t_login_handler:handler(DecodeData),
            {ok, State};
        2 ->
            ?LOG("333:~n"),
            {ok, State}
    end.

websocket_info(start, _ConnState, State) ->
    ?LOG("start arg:~p~n", [_ConnState]),
    {reply, {text, <<"erlang message received">>}, State}.

websocket_terminate(_Reason, _ConnState, _State) ->
    ?LOG("Websocket closed in state ~p wih reason ~p~n",
        [_State, _Reason]),
    ok.


ws_uniform() ->
    [A1, A2, A3] = get(?pack_random),
    B1 = (A1 * 171) rem 30269,
    B2 = (A2 * 172) rem 30307,
    B3 = (A3 * 170) rem 30323,
    put(?pack_random, [B1, B2, B3]),
    B1 + B2 + B3.

    