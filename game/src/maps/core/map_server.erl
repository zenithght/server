%%%-------------------------------------------------------------------
%%% @author yujian
%%% @doc 像素为单位
%%%
%%% Created : 23. 六月 2016 下午4:53
%%%-------------------------------------------------------------------

-module(map_server).

-behaviour(gen_server).

-include("pub.hrl").
-include("map_server.hrl").

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([refresh_fps/1]).

-record(state, {config}).

start_link(Arg) ->
    gen_server:start_link(?MODULE, Arg, [{'spawn_opt', [{priority, high},
%%        {scheduler, 0},
        {min_heap_size, 65536 * 24}, {min_bin_vheap_size, 65536 * 48}]}]).


init(Config) ->
    ?INFO("~p init self_pid:~p~n", [?MODULE, self()]),
    #map_queue{self_pid = QueuePid, map_type = MapType, rank_pid = MapRankPid, map_r = MapR, map_chunk_weight = MapChunkWeight} = Config,
    
    ?put_new(?map_r, MapR),
    ?put_new(?map_chunk_weight, MapChunkWeight),
    ?put_new(?rank_pid, MapRankPid),
    ?put_new(?queue_pid, QueuePid),
    
    process_flag(trap_exit, true),
    erlang:link(MapRankPid),
    
    map_chunks:init(),
    map_player:init(),
    map_foods:init(),
    
    map_plugin:init(MapType),
    
    erlang:start_timer(?tick_time, self(), ?timeout_tick),
    erlang:start_timer(?tick_rank, self(), ?timeout_tick_rank),

%%    erlang:start_timer(?tick_refresh_ai_food, self(), ?timeout_refresh_ai_food),
    ?put_new(?THIS_FPS, 1),
    ?put_new(?player_pids, []),
    {ok, #state{config = Config}}.


handle_call({mod, Mod, Msg}, _From, State) ->
    Mod:handle_call(Msg, _From, State);

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({timeout, _TimerRef, {skill_id, ?SKILL_1, PlayerId}}, State) ->
    map_player_set:set_player_state(PlayerId, #player.state, ?PLAYER_INIT_DONE),
    {noreply, State};

handle_info({timeout, _TimerRef, ?timeout_refresh_ai_food}, State) ->
    erlang:start_timer(?tick_refresh_ai_food, self(), ?timeout_refresh_ai_food),
    map_foods_ai:refresh_ai_foods(),
    {noreply, State};

handle_info({timeout, _TimerRef, ?timeout_tick_rank}, State) ->
    erlang:start_timer(?tick_rank, self(), ?timeout_tick_rank),
    sets:fold(
        fun(PlayerId, Acc) ->
            map_player_proto:proto_rank(get(?rank_pid), PlayerId),
            Acc
        end, [], get(?ids_player)),
    {noreply, State};

handle_info({timeout, _TimerRef, {?timeout_refresh_food, FoodRefreshTime}}, State) ->
    erlang:start_timer(FoodRefreshTime, self(), {?timeout_refresh_food, FoodRefreshTime}),
    map_foods:refresh_food(),
    {noreply, State};


handle_info({timeout, _TimerRef, {?timeout_refresh_food_2, FoodRefreshTime2}}, State) ->
    erlang:start_timer(FoodRefreshTime2, self(), {?timeout_refresh_food_2, FoodRefreshTime2}),
    map_foods:refresh_specil_food(),
    {noreply, State};

handle_info({timeout, _TimerRef, ?timeout_tick}, State) ->
    case get(?player_pids) of
        [] -> {stop, normal, State};
        _ ->
            erlang:start_timer(?tick_time, self(), ?timeout_tick),
            {noreply, State}
    end;

handle_info({timeout, _TimerRef, {?timeout_fps, _Msg}}, State) ->
    
    MiSec1 = erl_time:times(milli_seconds),
    
    NewState = refresh_fps(State),
    
    case get(?player_pids) of %% 当为空时当前帧要走完。
        [] ->
            {noreply, State};
        _ ->
            FpsTimes = trunc(1000 / ?FPS * 0.95), %每一帧需要在47毫秒内计算完成
            
            MiSec = erl_time:times(milli_seconds),
%%            ?PRINT("fps cost:~p~n", [(MiSec - MiSec1)]),
            if
                (MiSec - MiSec1) > FpsTimes ->
%%            ?PRINT("1 fps cost > 200ms:~p~n", [(MiSec - MiSec1)]),
                    erlang:start_timer(0, self(), {?timeout_fps, MiSec});
                true ->
                    erlang:start_timer(trunc(FpsTimes - (MiSec - MiSec1)), self(), {?timeout_fps, MiSec})
            end,
            {noreply, NewState}
    end;

handle_info(stop, State) ->
    {stop, normal, State};

handle_info({inet_reply, _Sock, _Error}, State) ->
    {noreply, State};

handle_info({mod, Mod, Msg}, State) ->
    Mod:handle_info(Msg, State);

handle_info(Info, State) ->
    ?INFO("info:~p~n", [Info]),
    map_plugin:handler_info(Info, State).
%%    {noreply, State}.


terminate(_Reason, _State) ->
    %% 当地图挂掉，发送消息给玩家进程
    ?INFO("map server terminate:~p~n", [[self(), _Reason]]),
    if
        _Reason =:= normal ->
            RankPid = get(?rank_pid),
            rank_map:refresh_rank(RankPid),
            
            sets:fold(
                fun(PlayerId, Acc) ->
                    map_player_proto:proto_rank(RankPid, PlayerId),
                    Acc
                end, [], get(?ids_player)),
            PlayerIds = get(?ids_player),
            DiePlayerIds = get(?ids_die_player),
            AllPlayer = sets:union(PlayerIds, DiePlayerIds),
            sets:fold(
                fun(PlayerId, Acc) ->
                    case map_player_set:get_player(PlayerId) of
                        [] -> [];
                        Player when Player#player.player_type =:= ?PLAYER_TYPE_AI ->
                            ai:ai_leave(Player#player.pid);
                        Player -> map_plugin:map_terminate(Player)
                    end,
                    Acc
                end,
                [],
                AllPlayer);
        true ->
            ok
    end,
    
    map_plugin:map_terminate(),
    
    rank_map:stop(get(?rank_pid)),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% 1.取出所有蛇，计算该蛇1.碰撞 2.运动 3.吃球 4.视野
refresh_fps(State) ->

%%    T1 = os:timestamp(),
    
    PlayerIds = get(?ids_player),
    
    ThisFps = get(?THIS_FPS),
    if
        ThisFps > ?FPS -> put(?THIS_FPS, 1);
        true -> put(?THIS_FPS, ThisFps + 1)
    end,
    
    ChunkWeight = get(?map_chunk_weight),
    MapR = get(?map_r),

%% @doc 初始化该帧信息，清空上一帧的信息
    map_player:refresh_fps_init(),

%% @doc 1.移动食物先移动完成
%%    map_foods_ai:refresh_fps_move(),

%%    T2 = os:timestamp(),
    
    %% @doc 计算所有蛇的移动
    sets:fold(fun(PlayerId, Acc) -> map_player:refresh_fps_move(ChunkWeight, PlayerId),
        Acc end, [], PlayerIds),

%%    T3 = os:timestamp(),

%% @doc 计算所有蛇的碰撞，撞到人
    sets:fold(fun(PlayerId, Acc) -> map_player:refresh_fps_collision(ChunkWeight, MapR, PlayerId),
        Acc end, [], PlayerIds),

%%    T4 = os:timestamp(),
    
    %% @doc 计算吃食物
    sets:fold(fun(PlayerId, Acc) -> map_player:refresh_fps_eat_food(ChunkWeight, PlayerId),
        Acc end, [], PlayerIds),

%%    T5 = os:timestamp(),
    
    sets:fold(fun(PlayerId, Acc) -> map_player:refresh_fps_view(ChunkWeight, MapR, PlayerId),
        Acc end, [], PlayerIds),

%% @doc 计算视野, 清理掉死掉的蛇
    NewPlayerPids = sets:fold(
        fun(PlayerId, Acc) ->
            case map_player_set:get_player(PlayerId) of
                [] -> Acc;
                Player ->
                    PlayerState = Player#player.state,
                    if
                        PlayerState =:= ?PLAYER_LEAVE_MAP ->
                            map_player:leave(PlayerId, Player),
                            Acc;
                        PlayerState =:= ?PLAYER_STATE_DIE ->
                            map_player:die(PlayerId, Player),
                            Acc;
                        true ->
                            sets:add_element(PlayerId, Acc)
                    end
            end
        end,
        sets:new(),
        PlayerIds),

%%    T6 = os:timestamp(),
    
    %% @doc 死亡玩家不移除地图，直到玩家离开地图
    NewDieids = sets:fold(
        fun(DiePlayerId, Acc) ->
            case map_player_set:get_player(DiePlayerId) of
                [] -> Acc;
                DiePlayer ->
                    PlayerState = DiePlayer#player.state,
                    if
                        PlayerState =:= ?PLAYER_LEAVE_MAP ->
                            map_player_set:del_player(DiePlayerId),
                            rank_map:del(get(?rank_pid), DiePlayerId),
                            map_plugin:leave_map(DiePlayer),
                            
                            Acc;
                        PlayerState =:= ?PLAYER_STATE_DIE ->
                            sets:add_element(DiePlayerId, Acc);
                        true ->
                            Acc
                    end
            end
        end,
        sets:new(),
        get(?ids_die_player)),

%%    T7 = os:timestamp(),
    
    put(?ids_die_player, NewDieids),
    
    map_player:refresh_fps_clear(),
    
    put(?ids_player, NewPlayerPids),

%%    T8 = os:timestamp(),

%%    ?PRINT("map_server micro_second:~p~ndiff1:~p~ndiff2:~p~ndiff3:~p~ndiff4:~p~ndiff5:~p~ndiff6:~p~ndiff7:~p~n", [timer:now_diff(T8, T1), timer:now_diff(T2, T1), timer:now_diff(T3, T2), timer:now_diff(T4, T3), timer:now_diff(T5, T4), timer:now_diff(T6, T5), timer:now_diff(T7, T6), timer:now_diff(T8, T7)]),
    
    State.
