%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%%
%%% Created : 23. 八月 2016 下午2:04
%%%-------------------------------------------------------------------
-module(ai_init).

-include("pub.hrl").
-include("map_server.hrl").
-include("../ai_player.hrl").

-export([handle_info/2]).

handle_info({?ai_enter_map, PlayerId, EtsPlayer, EtsFood}, State) ->
    queue_arena:ai_enter(State#ai_player.queue_pid, State#ai_player.map_pid),
    
    erlang:start_timer(0, self(), ?timeout_fps),
    {noreply, State#ai_player{state = ?AI_PLAYER_ENTER_MAP, state_1 = ?AI_PLAYER_PLAY, player_id = PlayerId, ets_player = EtsPlayer, ets_food = EtsFood}};

handle_info(?ai_enter_map, State) ->
    erlang:start_timer(0, self(), ?timeout_fps),
    
    {noreply, State#ai_player{state = ?AI_PLAYER_ENTER_MAP, state_1 = ?AI_PLAYER_PLAY}};

handle_info(_Msg, _State) ->
    ?ERROR("cannot match msg:~p~n", [[_Msg, _State]]),
    {noreply, _State}.
