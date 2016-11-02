%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%% 行为层状态机。 加速、设置角度
%%% Created : 23. 八月 2016 下午2:04
%%%-------------------------------------------------------------------
-module(ai_behaviour).

-include("pub.hrl").
-include("map_server.hrl").
-include("../ai_player.hrl").

-export([st_move/3]).

%% @doc 计算方向
%% 1.先吃死亡食物
%% 2.大食物
%% 3.圆心方向的所有食物
st_move(State, FoodSets, ObstacleDict) ->
    PlayerId = State#ai_player.player_id,
    case catch ets:lookup(State#ai_player.ets_player, PlayerId) of
        [Player] ->
            #obstacle{x = HeadX, y = HeadY, r = HeadR} = Player#player.head,
            Size = dict:size(ObstacleDict),
            State1 = if
                         Size =:= 0 ->
                             st_move_no_player(State, PlayerId, Player, HeadX, HeadY, HeadR, FoodSets, ?no_player);
                         true ->
                             st_move_has_player(State, PlayerId, Player, HeadX, HeadY, HeadR, FoodSets, ObstacleDict)
                     end,
            {noreply, State1};
        
        _ -> {stop, normal, State}
    end.


st_move_no_player(State, PlayerId, Player, HeadX, HeadY, HeadR, FoodIds, HasPlayer) ->
    EtsFood = State#ai_player.ets_food,
    {DieFoods, Lv4upFoods, CenFoods} = ai_mod:parse_food(EtsFood, HeadX, HeadY, FoodIds),
    StEatFood =
        case ai_basic:st_eat_food(?move_eat_food_1, {Player, HeadX, HeadY, HeadR, DieFoods}) of
            error ->
                case ai_basic:st_eat_food(?move_eat_food_2, {Player, HeadX, HeadY, HeadR, Lv4upFoods}) of
                    error -> ai_basic:st_eat_food(?move_eat_food_3, {Player, HeadX, HeadY, HeadR, CenFoods, HasPlayer});
                    V2 -> V2
                end;
            V1 -> V1
        end,
    
    {Angle, IsSpeed, CostFps, Move} =
        case StEatFood of
            error -> ai_basic:st_move(?move_to_center_1, {HeadX, HeadY});
            V4 -> V4
        end,

%%    ?DEBUG("111:~p~n", [[PlayerId, round(Angle), IsSpeed, CostFps, Move]]),
    map:update_player_angle(State#ai_player.map_pid, PlayerId, round(Angle), IsSpeed),
    State#ai_player{angle = Angle, is_speed = IsSpeed, move = Move, cost_fps = CostFps, state_1 = ?AI_PLAYER_PLAY}.


st_move_has_player(State, PlayerId, Player, HeadX, HeadY, HeadR, FoodSets, ObstacleDict) ->
    {Len, _PlayerId1, BodyLen, BodyX, BodyY} = ai_mod:parse_obstacle(PlayerId, HeadX, HeadY, HeadR, ObstacleDict),
    if
        Len =< 0 orelse Len >= 100 ->
            st_move_no_player(State, PlayerId, Player, HeadX, HeadY, HeadR, FoodSets, ?has_player);
        true ->
            %%bodylen =< 10 找蛇头 溜一圈
            %%bodylen > 10 找下一截身体方向，大于等于平行线移动
            
            {Angle, IsSpeed, CostFps, Move} =
                if
                    BodyLen =< 5 -> %反方向加速逃跑 10-60帧加速
%%                        Head = dict:find({_PlayerId1, 1}, ObstacleDict),
%%                        Body1 = dict:find({_PlayerId1, 2}, ObstacleDict),
%%
%%                        CostFps1 = erl_random:random(10, 60),
%%                        {map_math:xy_to_angle(BodyX, BodyY, HeadX, HeadY), erl_random:random(2) - 1, CostFps1, {?move_fight_1, BodyX, BodyY, HeadX, HeadY}};
                        ai_basic:st_move(?move_escape_1, {BodyX, BodyY, HeadX, HeadY});
                    BodyLen =< 10 ->
                        ai_basic:st_move(?move_escape_1, {BodyX, BodyY, HeadX, HeadY});
                    
                    true -> %身体部位 后方向逃跑
                        GetMaxLen =
                            dict:fold(
                                fun({DPlayerId, DBodyLen}, {DX, DY, DR}, {DBodyLenAcc, DXAcc, DYAcc, DRAcc}) ->
                                    if
                                        PlayerId =:= DPlayerId ->
                                            if
                                                DBodyLen =< BodyLen -> {DBodyLenAcc, DXAcc, DYAcc, DRAcc};
                                                DBodyLenAcc >= DBodyLen -> {DBodyLenAcc, DXAcc, DYAcc, DRAcc};
                                                true -> {DBodyLen, DX, DY, DR}
                                            end;
                                        true ->
                                            {DBodyLenAcc, DXAcc, DYAcc, DRAcc}
                                    end
                                end,
                                {0, 0, 0, 0},
                                ObstacleDict),
                        case GetMaxLen of
                            {0, 0, 0, 0} ->
                                ai_basic:st_move(?move_escape_1, {BodyX, BodyY, HeadX, HeadY});
                            {_, MaxX, MaxY, _} ->
                                ai_basic:st_move(?move_escape_2, {BodyX, BodyY, MaxX, MaxY})
                        end
                end,
%%            ?DEBUG("222:~p~n", [[PlayerId, HeadX, HeadY, round(Angle), IsSpeed]]),
            map:update_player_angle(State#ai_player.map_pid, PlayerId, round(Angle), IsSpeed),
            State#ai_player{angle = Angle, is_speed = IsSpeed, move = Move, cost_fps = CostFps, state_1 = ?AI_PLAYER_PLAY}
    end.