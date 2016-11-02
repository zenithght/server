%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%%
%%% Created : 19. 八月 2016 下午2:30
%%%-------------------------------------------------------------------
-module(ai_player).

-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-include("pub.hrl").
-include("ai_player.hrl").
-include("map_server.hrl").

start_link(AiPlayer) ->
    gen_server:start_link(?MODULE, AiPlayer, []).


init(AiPlayer) ->
    process_flag(trap_exit, true),
    Name = config_map_ai_name:random(),
    Model = config_skin:random(),
    map:ai_enter_map(AiPlayer#ai_player.map_pid, self(), Name, Model),
    {ok, AiPlayer}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


%% 当进程中没有玩家后，机器人不做任何动作
handle_info(?ai_get_chunks_info, State) ->
    {noreply, State#ai_player{state_1 = ?AI_PLAYER_SUSPEND}};

%% 当地图进程激活后，重新计算fps
handle_info(?ai_activate_map_server, State) ->
    if
        State#ai_player.state_1 =:= ?AI_PLAYER_SUSPEND ->
            erlang:start_timer(0, self(), ?timeout_fps);
        true ->
            ok
    end,
    {noreply, State#ai_player{state_1 = ?AI_PLAYER_PLAY}};

handle_info({?ai_get_chunks_info, FoodSets, ObstacleDict, MiSec1}, State) ->
    if
        State#ai_player.state =:= ?AI_PLAYER_ENTER_MAP ->
            Ret = if
                      State#ai_player.cost_fps =< 0 ->
                          ai_behaviour:st_move(State, FoodSets, ObstacleDict);
                      true ->
                          {noreply, State#ai_player{cost_fps = State#ai_player.cost_fps - 1, state_1 = ?AI_PLAYER_PLAY}}
                  end,
            
            MiSec = erl_time:m_now(),
            FpsTimes = trunc(1000 / ?FPS),
            
            if
                (MiSec - MiSec1) > FpsTimes ->
                    erlang:start_timer(0, self(), ?timeout_fps);
                true ->
                    erlang:start_timer(trunc(FpsTimes - (MiSec - MiSec1)), self(), ?timeout_fps)
            end,
            Ret;
        State#ai_player.state =:= ?AI_PLAYER_DIE ->
            {noreply, State};
        true ->
            ?DEBUG("111:~p~n", [[State#ai_player.player_id, State#ai_player.state]]),
            {stop, normal, State}
    end;

handle_info({timeout, _TimerRef, ?timeout_fps}, State) ->
    if
        State#ai_player.state_1 =:= ?AI_PLAYER_SUSPEND ->
            ?DEBUG("111:~p~n", [[State#ai_player.player_id, State#ai_player.state, State#ai_player.state_1]]),
            {noreply, State};
        State#ai_player.state =:= ?AI_PLAYER_ENTER_MAP ->
            MiSec1 = erl_time:times(milli_seconds),
            refresh_fps(MiSec1, State);
        
        State#ai_player.state =:= ?AI_PLAYER_DIE ->
            {noreply, State};
        true ->
            ?DEBUG("222:~p~n", [[State#ai_player.player_id, State#ai_player.state, State#ai_player.state_1]]),
            {noreply, State}
    end;

handle_info({mod, Mod, Msg}, State) ->
    Mod:handle_info(Msg, State);

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    AiState = _State#ai_player.state,
    
    if
        AiState =/= ?AI_PLAYER_INIT ->
            queue_arena:ai_leave(_State#ai_player.queue_pid, _State#ai_player.map_pid),
            map:ai_leave_map(_State#ai_player.map_pid, _State#ai_player.player_id);
        true -> ok
    end.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% 是否移动， 是否移动到边界 是否避让 是否加速 是否杀人....
refresh_fps(MiSec1, State) ->
    if
        State#ai_player.state =:= ?AI_PLAYER_ENTER_MAP ->
            PlayerId = State#ai_player.player_id,
            case catch ets:lookup(State#ai_player.ets_player, PlayerId) of
                [Player] ->
                    MapPid = State#ai_player.map_pid,
                    case erlang:is_process_alive(MapPid) of
                        true ->
                            #obstacle{in_chunks = Chunk} = Player#player.head,
                            map:ai_get_chunks_info(MapPid, self(), Chunk, MiSec1),
                            {noreply, State#ai_player{state_1 = ?AI_PLAYER_WAITING}};
                        false ->
                            {stop, normal, State}
                    end;
                _ -> {stop, normal, State}
            end;
        true ->
            {stop, normal, State}
    end.
    