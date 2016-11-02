%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc 回调事件插件
%%%
%%% Created : 03. 八月 2016 上午10:41
%%%-------------------------------------------------------------------
-module(map_plugin).

-include("pub.hrl").
-include("map_server.hrl").

-export([handler_info/2]).

-export([init/1, enter_map/1, leave_map/1, map_terminate/1, map_terminate/0]).

-export([set_plugin/1, del_plugin/0]).

-callback map_init() -> _.

-callback enter_map(#player{}) -> _.

%% @doc 玩家主動離開場景時調用，此時玩家已經不在場景中
-callback leave_map(#player{}) -> _.

%%%% @doc 玩家死亡时回调
%%-callback die(Self :: #player{}) -> _.
%%
%%%% @doc 杀死一个agent时调用, 这是被杀死的agent已经是死亡状态,或者已经离开场景
%%-callback kill_agent(Self :: #player{}, Kill :: #player{}) -> _.

-callback map_terminate(Player :: #player{}) -> _.

-callback map_terminate() -> _.

init(MapType) ->
    case MapType of
        ?MAP_NORMAL -> map_normal:map_init();
        ?MAP_ARENA -> map_arena:map_init();
        ?MAP_ROOM -> map_room:map_init()
    end.


set_plugin(Mod) when is_atom(Mod) ->
    put(?pd_player_plugin, Mod).


del_plugin() ->
    erlang:erase(?pd_player_plugin).


enter_map(Player) ->
    case get(?pd_player_plugin) of
        ?undefined -> ok;
        Plugin -> Plugin:enter_map(Player)
    end.

leave_map(Player) ->
    case get(?pd_player_plugin) of
        ?undefined -> ok;
        Plugin -> Plugin:leave_map(Player)
    end.


map_terminate(Player) ->
    case get(?pd_player_plugin) of
        ?undefined -> ok;
        Plugin -> Plugin:map_terminate(Player)
    end.


map_terminate() ->
    case get(?pd_player_plugin) of
        ?undefined -> ok;
        Plugin -> Plugin:map_terminate()
    end.


handler_info(Info, State) ->
    case get(?pd_player_plugin) of
        ?undefined -> ok;
        Plugin -> Plugin:handler_info(Info, State)
    end.