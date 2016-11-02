%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%% 基础层状态机。 向某个方向移动
%%% Created : 23. 八月 2016 下午2:04
%%%-------------------------------------------------------------------
-module(ai_basic).

-include("pub.hrl").
-include("map_server.hrl").
-include("../ai_player.hrl").

-export([
    st_move/2,
    st_fight/2,
    st_eat_food/2
]).

st_move(?move_to_center_1, {X1, Y1}) ->
    CostFps1 = erl_random:random(?RANDOM_FPS),
    {map_math:xy_to_angle(X1, Y1, 0, 0), ?PLAYER_NORMAL_SPEED, CostFps1, {?move_to_center_1, X1, Y1}};

st_move(?move_to_center_2, {X1, Y1}) ->
    CostFps1 = erl_random:random(?RANDOM_FPS),
    {map_math:xy_to_angle(X1, Y1, 0, 0), ?PLAYER_SPEED_UP, CostFps1, {?move_to_center_2, X1, Y1}};

st_move(?move_to_center_3, {X1, Y1}) ->
    CostFps1 = erl_random:random(?RANDOM_FPS),
    {map_math:xy_to_angle(X1, Y1, 0, 0), erl_random:random(2) - 1, CostFps1, {?move_to_center_3, X1, Y1}};


st_move(?move_escape_1, {X1, Y1, X2, Y2}) ->
    CostFps1 = erl_random:random(40),
    {map_math:xy_to_angle(X1, Y1, X2, Y2), ?PLAYER_NORMAL_SPEED, CostFps1, {?move_escape_1, X1, Y1, X2, Y2}};

%% @doc 远离某点的方向,1帧
st_move(?move_escape_2, {X1, Y1, X2, Y2}) ->
    {map_math:xy_to_angle(X1, Y1, X2, Y2), ?PLAYER_NORMAL_SPEED, erl_random:random(40), {?move_escape_2, X1, Y1, X2, Y2}}.

st_fight(?move_fight_1, {_Player, MyX, MyY, {Body1X, Body1Y, _R1}, {Body2X, Body2Y, _R2}}) ->
%%    MoveLen = (Player#player.config_speed_up / ?FPS),
%%    BodyLen = map_math:far(Body1X, Body1Y, MyX, MyY),
%%    Distance = math:sqrt(BodyLen) - R1,
%%    Distance / MoveLen,
    
    {map_math:xy_to_angle(Body1X, Body1Y, Body2X, Body2Y), ?PLAYER_SPEED_UP, erl_random:random(10), {?move_fight_1, MyX, MyY}}.


st_eat_food(?move_eat_food_1, {Player, HeadX, HeadY, HeadR, DieFoods}) ->
    case min_item(HeadX, HeadY, DieFoods) of
        {0, 0, 0, 0} -> error;
        {BodyLen, FoodX, FoodY, FoodR} ->
            Len = length(DieFoods),
            {IsSpeed, CostFps} =
                if
                    Len >= 20 ->
                        Distance = math:sqrt(BodyLen) - FoodR,
                        MoveLen = (Player#player.config_speed_up / ?FPS),
                        {?PLAYER_SPEED_UP, trunc((Distance - (HeadR * 1.5) * (1 + Distance)) / MoveLen) + 1};
                    true ->
                        {?PLAYER_NORMAL_SPEED, erl_random:random(?RANDOM_FPS)}
                end,
%%            Distance = math:sqrt(BodyLen) - FoodR,
%%            MoveLen = (Player#player.config_speed_up / ?FPS),
%%            CostFps = trunc((Distance - (HeadR * 1.5) * (1 + Distance)) / MoveLen) + 1,
            {map_math:xy_to_angle(HeadX, HeadY, FoodX, FoodY), IsSpeed, CostFps, {?move_eat_food_1, HeadX, HeadY, FoodX, FoodY}}
    end;

st_eat_food(?move_eat_food_2, {_Player, HeadX, HeadY, _HeadR, Lv4upFoods}) ->
    case min_item(HeadX, HeadY, Lv4upFoods) of
        {0, 0, 0, 0} -> error;
        {_BodyLen, FoodX, FoodY, _FoodR} ->
%%            Distance = math:sqrt(BodyLen) - FoodR,
%%            MoveLen = (Player#player.config_speed_up / ?FPS),
%%            CostFps = trunc((Distance - (HeadR * 1.5) * (1 + Distance)) / MoveLen) + 1,
            CostFps = erl_random:random(?RANDOM_FPS),
            {map_math:xy_to_angle(HeadX, HeadY, FoodX, FoodY), ?PLAYER_NORMAL_SPEED, CostFps, {?move_eat_food_2, HeadX, HeadY, FoodX, FoodY}}
    end;

st_eat_food(?move_eat_food_3, {Player, HeadX, HeadY, HeadR, CenFoods, HasPlayer}) ->
    case min_item(HeadX, HeadY, CenFoods) of
        {0, 0, 0, 0} -> error;
        {BodyLen, FoodX, FoodY, FoodR} ->
            Len = length(CenFoods),
            {IsSpeed, CostFps} =
                if
                    Len >= 20 ->
                        if
                            HasPlayer =:= ?no_player ->
                                Distance = math:sqrt(BodyLen) - FoodR,
                                MoveLen = (Player#player.config_speed_up / ?FPS),
                                {?PLAYER_SPEED_UP, trunc((Distance - (HeadR * 1.5) * (1 + Distance)) / MoveLen) + 1};
                            true ->
                                {?PLAYER_NORMAL_SPEED, erl_random:random(?RANDOM_FPS)}
                        end;
                    true ->
                        {?PLAYER_NORMAL_SPEED, erl_random:random(?RANDOM_FPS)}
                end,
            {map_math:xy_to_angle(HeadX, HeadY, FoodX, FoodY), IsSpeed, CostFps, {?move_eat_food_3, HeadX, HeadY, FoodX, FoodY}}
    end.


min_item(_HeadX, _HeadY, []) -> {0, 0, 0, 0};
min_item(HeadX, HeadY, Foods) ->
    FunFoldl =
        fun({X, Y, R}, {Len, XAcc, YAcc, RAcc}) ->
            NewLen = map_math:far(HeadX, HeadY, X, Y),
            if
                Len =:= 0 -> {NewLen, X, Y, R};
                NewLen =< Len -> {NewLen, X, Y, R};
                true -> {Len, XAcc, YAcc, RAcc}
            end
        end,
    lists:foldl(FunFoldl, {0, 0, 0, 0}, Foods).
    