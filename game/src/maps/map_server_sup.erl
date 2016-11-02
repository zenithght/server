%%%-------------------------------------------------------------------
%%% @author yujian
%%% @doc
%%%
%%% Created : 23. 六月 2016 下午4:54
%%%-------------------------------------------------------------------
-module(map_server_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-export([start_child/2, start_arena_map/1, start_arena_map/2, start_child/1]).

-include("pub.hrl").

-include("map_server.hrl").

-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    Child = [
        ?CHILD(map_server, worker)
    ],
    {ok, {{simple_one_for_one, 5, 10}, Child}}.


start_child(MapQueue) ->
    {ok, MapRankPid} = rank_map_sup:start_child([MapQueue]),    %启动对应的排行榜进程
    supervisor:start_child(?MODULE, [MapQueue#map_queue{rank_pid = MapRankPid}]).


start_child(MapR, MapChunkWeight) ->
    Config = #map_queue{map_r = MapR, map_chunk_weight = MapChunkWeight},
    {ok, MapRankPid} = rank_map_sup:start_child([Config]),    %启动对应的排行榜进程
    supervisor:start_child(?MODULE, [Config#map_queue{rank_pid = MapRankPid}]).



start_arena_map(MapR, MapChunkWeight) ->
    Config = #map_queue{map_r = MapR, map_chunk_weight = MapChunkWeight, map_type = ?MAP_ARENA},
    {ok, MapRankPid} = rank_map_sup:start_child([Config]),    %启动对应的排行榜进程
    supervisor:start_child(?MODULE, [Config#map_queue{rank_pid = MapRankPid}]).


start_arena_map(MatchMapQueue) ->
    {ok, MapRankPid} = rank_map_sup:start_child([MatchMapQueue]),    %启动对应的排行榜进程
    supervisor:start_child(?MODULE, [MatchMapQueue#map_queue{rank_pid = MapRankPid}]).