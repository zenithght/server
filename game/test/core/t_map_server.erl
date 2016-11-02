%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%%
%%% Created : 03. 九月 2016 上午10:59
%%%-------------------------------------------------------------------
-module(t_map_server).

-export([start/0]).

start() ->
    {ok, MapPid} = map_server_sup:start_arena_map(5000, 1000),
    ai_sup:start_ai(MapPid, 5000, 30).