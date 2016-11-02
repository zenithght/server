%%%-------------------------------------------------------------------
%%% @author yujian
%%% @doc
%%% Created : 15. 十二月 2015 下午2:45
%%%-------------------------------------------------------------------
-module(ai_sup).

-behaviour(supervisor).

-include("pub.hrl").
-include("ai_player.hrl").

-export([start_link/0]).

-export([init/1, start/1, start_ai/4]).

-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    Child = [
        ?CHILD(ai_player, worker)
    ],
    {ok, {{simple_one_for_one, 5, 10}, Child}}.

start_child(QueuePid, MapPid, MapR) ->
    supervisor:start_child(?MODULE, [#ai_player{queue_pid = QueuePid, map_pid = MapPid, map_r = MapR}]).

start(MapPid) ->
    start_ai(undefined, MapPid, 5000, 1).

start_ai(QueuePid, MapPid, MapR, Num) ->
    [start_child(QueuePid, MapPid, MapR) || _I <- lists:seq(1, Num)].