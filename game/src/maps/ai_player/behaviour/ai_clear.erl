%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%%
%%% Created : 23. 八月 2016 下午2:04
%%%-------------------------------------------------------------------
-module(ai_clear).

-include("pub.hrl").
-include("map_server.hrl").
-include("../ai_player.hrl").

-export([handle_info/2]).


handle_info(ai_player_die, State) ->
    PlayerNum = queue_arena:get_map_num(State#ai_player.queue_pid, State#ai_player.map_pid),
    if
        PlayerNum > 20 ->
            {stop, normal, State};
        true ->
            timer:sleep(3000),
            MapPid = State#ai_player.map_pid,
            case erlang:is_process_alive(MapPid) of
                true ->
                    map:ai_enter_map(State#ai_player.map_pid, self(), State#ai_player.player_id),
                    {noreply, State#ai_player{state = ?AI_PLAYER_DIE}};
                _ ->
                    {stop, normal, State}
            end
    end;


handle_info(ai_player_leave, State) ->
    {stop, normal, State#ai_player{state = ?AI_PLAYER_LEAVE}};

handle_info(_Msg, _State) ->
    ?ERROR("cannot match msg:~p~n", [[_Msg, _State]]),
    {noreply, _State}.