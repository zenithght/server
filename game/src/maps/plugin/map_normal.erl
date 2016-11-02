%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%%
%%% Created : 03. 八月 2016 上午10:41
%%%-------------------------------------------------------------------
-module(map_normal).

-include("pub.hrl").
-include("map_plugin.hrl").
-include("map_server.hrl").

handler_info(_Info, State) -> {noreply, State}.


map_init() ->
    map_plugin:set_plugin(?MODULE).

enter_map(Player) -> Player.

leave_map(_Player) ->
    ?LOG("player leave map:~p~n", [_Player#player.id]),
    map_normal_manager:del_player(self()).


map_terminate(Player) -> Player.


map_terminate() ->
    map_normal_manager:del_map(self()).