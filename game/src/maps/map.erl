%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%%
%%% Created : 01. 七月 2016 上午9:29
%%%-------------------------------------------------------------------
-module(map).

-include("pub.hrl").
-include("map_server.hrl").

-export([
    enter_map/2, enter_map/6, player_kill_leave/2, player_leave/2,
    
    update_player_angle/4,
    update_player_pos/5,
    
    watch_join_map/3, watch_join_map/4, watch_leave_map/2,

%%    update_player_speed/3,
    
    ai_enter_map/4, ai_enter_map/3,
    ai_leave_map/2,
    ai_get_chunks_info/4
]).

enter_map(MapPid, PlayerId) ->
    MapPid ! {mod, ?map_handler, {?player_enter_map, PlayerId}}.

enter_map(MapPid, PlayerPid, Uid, Socket, Name, Model) ->
    % 随机一个位置,要求该位置500*500范围内没有障碍物
    gen_server:call(MapPid, {mod, ?map_handler, {?player_enter_map, PlayerPid, Uid, Socket, Name, Model}}).

%% @doc 在地图中
watch_join_map(MapPid, PlayerPid, FriendMapId, MyId) ->
    MapPid ! {mod, ?map_handler, {?watch_player_join_map, PlayerPid, FriendMapId, MyId}}.

%% @doc 不在地图中
watch_join_map(MapPid, PlayerPid, FriendMapId) ->
    MapPid ! {mod, ?map_handler, {?watch_player_join_map, PlayerPid, FriendMapId}}.

%% @doc 玩家掉线或者离开的情况
watch_leave_map(MapPid, PlayerId) ->
    gen_server:call(MapPid, {mod, ?map_handler, {?watch_player_leave_map, PlayerId, self()}}).

update_player_angle(MapPid, PlayerId, Angle, IsSpeed) ->
    MapPid ! {mod, ?map_handler, {?set_player_angle, PlayerId, Angle, IsSpeed}}.

update_player_pos(MapPid, PlayerId, X, Y, IsSpeed) ->
    MapPid ! {mod, ?map_handler, {?set_player_pos, PlayerId, X, Y, IsSpeed}}.


%% @doc 玩家掉线或者离开的情况
player_kill_leave(MapPid, PlayerId) ->
    gen_server:call(MapPid, {mod, ?map_handler, {?player_kill_leave, PlayerId}}).

player_leave(MapPid, PlayerId) ->
    MapPid ! {mod, ?map_handler, {?player_leave, PlayerId}}.

%%update_player_speed(MapPid, PlayerId, IsSpeed) ->
%%    MapPid ! {?update_player_speed, PlayerId, IsSpeed}.


ai_enter_map(MapPid, PlayerPid, PlayerId) ->
    MapPid ! {mod, ?map_ai_handler, {?ai_enter_map, PlayerPid, PlayerId}}.

ai_enter_map(MapPid, PlayerPid, Name, Model) ->
    MapPid ! {mod, ?map_ai_handler, {?ai_enter_map, PlayerPid, Name, Model}}.


ai_leave_map(MapPid, PlayerId) ->
    MapPid ! {mod, ?map_ai_handler, {?ai_leave_map, PlayerId}}.


ai_get_chunks_info(MapPid, PlayerPid, Chunk, MiSec1) ->
    MapPid ! {mod, ?map_ai_handler, {?ai_get_chunks_info, PlayerPid, Chunk, MiSec1}}.



