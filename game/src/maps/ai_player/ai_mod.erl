%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%%
%%% Created : 19. 八月 2016 下午2:30
%%%-------------------------------------------------------------------
-module(ai_mod).

-export([parse_food/4, parse_obstacle/5]).

-include("pub.hrl").
-include("ai_player.hrl").
-include("map_server.hrl").


parse_food(EtsFood, _HeadX, _HeadY, FoodSets) ->
    FunFoldl =
        fun(FoodId, {DieFoodAcc, Lv4upFoodAcc, CenFoodAcc}) ->
            case ets:lookup(EtsFood, FoodId) of
                [] -> {DieFoodAcc, Lv4upFoodAcc, CenFoodAcc};
                [#food{x = X, y = Y, lv = 0, r = R}] ->
                    {[{X, Y, R} | DieFoodAcc], Lv4upFoodAcc, CenFoodAcc};
                [#food{x = X, y = Y, lv = Lv, r = R}] ->
                    if
                        Lv >= 4 -> {DieFoodAcc, [{X, Y, R} | Lv4upFoodAcc], CenFoodAcc};
                        true ->
                            {DieFoodAcc, Lv4upFoodAcc, [{X, Y, R} | CenFoodAcc]}
                    end
            end
        end,
        catch sets:fold(FunFoldl, {[], [], []}, FoodSets).


parse_obstacle(MyPlayerId, HeadX, HeadY, HeadR, ObstacleDict) ->
    FunFoldl =
        fun({PlayerId, BodyLen}, {X, Y, R}, {Len, PlayerIdAcc, BodyLenAcc, XAcc, YAcc, RAcc}) ->
            NewLen = map_math:far(HeadX, HeadY, X, Y),
            if
                MyPlayerId =:= PlayerId -> {Len, PlayerIdAcc, BodyLenAcc, XAcc, YAcc, RAcc};
                NewLen =< Len -> {NewLen, PlayerId, BodyLen, X, Y, R};
                Len =:= 0 -> {NewLen, PlayerId, BodyLen, X, Y, R};
                true -> {Len, PlayerIdAcc, BodyLenAcc, XAcc, YAcc, RAcc}
            end
        end,
    {MinLen, MinPlayerId, MinBodyLen, MinX, MinY, MinR} = dict:fold(FunFoldl, {0, 0, 0, 0, 0, 0}, ObstacleDict),
    
    {math:sqrt(MinLen) - HeadR - MinR, MinPlayerId, MinBodyLen, MinX, MinY}.
