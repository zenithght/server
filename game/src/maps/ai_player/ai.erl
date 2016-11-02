%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%%
%%% Created : 23. 八月 2016 上午11:11
%%%-------------------------------------------------------------------
-module(ai).

-include("pub.hrl").
-include("map_server.hrl").

-export([
    cb_enter_map/1,
    cb_enter_map/4,
    
    ai_die/1,
    ai_leave/1
]).

cb_enter_map(PlayerPid, PlayerId, EtsPlayer, EtsFood) ->
    PlayerPid ! {mod, ?ai_init, {?ai_enter_map, PlayerId, EtsPlayer, EtsFood}}.

cb_enter_map(PlayerPid) ->
    PlayerPid ! {mod, ?ai_init, ?ai_enter_map}.

ai_die(PlayerPid) ->
    PlayerPid ! {mod, ?ai_clear, ai_player_die}.

ai_leave(PlayerPid) ->
    PlayerPid ! {mod, ?ai_clear, ai_player_leave}.