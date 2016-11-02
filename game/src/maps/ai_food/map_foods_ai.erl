%%%-------------------------------------------------------------------
%%% @author yujian
%%% @doc 移动食物
%%% 1.可以穿过身体
%%% 2.可以跑出地图外
%%% 3.出生自带移动方向（离圆心远，向圆心移动，离圆心近，向圆外移动）
%%% 4.遇到蛇头向反方向逃跑
%%% 5.有警戒范围
%%% Created : 23. 六月 2016 下午4:53
%%%-------------------------------------------------------------------

-module(map_foods_ai).

-include("pub.hrl").
-include("map_server.hrl").

-export([init/0,
    refresh_fps_move/0,
    refresh_ai_foods/0,
    del_ai_food/3
]).


%% 移动食物也归入食物， 蛇吃移动食物和食物的逻辑一样。所以移动食物需要实时计算坐标，归入map_ets_foods表
init() ->
    erlang:put(?ids_ai_food, sets:new()),
    put(?chunks_ai_food, dict:new()).

%%    random_food(600, 5, 1, 1, sets:new(), dict:new()).

%%    #{ai_food_view_r := ViewR, ai_food_lv := FoodLv, max_num := MaxNum, model_max_num := ModelMaxNum} = config_global:get(<<"ai_food_config">>),
%%    Num = erl_random:random(MaxNum),
%%    random_food(ViewR, FoodLv, ModelMaxNum, Num, sets:new(), dict:new()).


random_food(ViewR, FoodLv, ModelMaxNum, Num, FoodAiIds, FoodAiChunks) ->
    ChunkWeight = get(?map_chunk_weight),
    MapR = get(?map_r),
    EtsFoods = get(?map_ets_foods),
    
    AutoId = get(?food_auto_id),
    
    FunFoldl =
        fun(I, {RecordAcc, SetsAcc, FoodAiChunksAcc}) ->
            NewX = erl_random:random(MapR + MapR) - MapR,
            NewY = erl_random:random(MapR + MapR) - MapR,
            
            Chunk = map_math:get_xyz(NewX, NewY, ChunkWeight),
            ModelNum = erl_random:random(ModelMaxNum),
            {R, Score} = config_map_ai_food_lv:get(FoodLv),
            
            Angle = map_math:xy_to_angle(NewX, NewY, 0, 0),
            
            case map_math:in_c(NewX, NewY, MapR) of
                true ->
                    Id = AutoId + I,
                    NewFoodAiChunksAcc = map_chunks:append_ai_food_chunks(Chunk, Id, FoodAiChunksAcc),
                    {
                        [
                            #ai_food{
                                id = Id,
                                x = NewX,
                                y = NewY,
                                lv = FoodLv,
                                view_r = ViewR,
                                r = R,
                                score = Score,
                                model = ModelNum,
                                target_a = round(Angle),
                                cur_a = round(Angle),
                                in_chunk = Chunk
                            } | RecordAcc],
                        sets:add_element(Id, SetsAcc),
                        NewFoodAiChunksAcc
                    };
                false ->
                    {RecordAcc, SetsAcc, FoodAiChunksAcc}
            end
        end,
    {Records, Sets, RetFoodAddAiIds} = lists:foldl(FunFoldl, {[], FoodAiIds, FoodAiChunks}, lists:seq(1, Num)),
    put(?ids_ai_food, Sets),
    put(?chunks_ai_food, RetFoodAddAiIds),
    put(?food_auto_id, AutoId + Num + 1),
    ets:insert(EtsFoods, Records).


%% 1.判断视野内玩家，反方向逃跑,计算移动方向
%% 2.移动
refresh_fps_move() ->
    ChunkWeight = get(?map_chunk_weight),
    MapR = get(?map_r),
    EtsFoods = get(?map_ets_foods),
    
    AllAiFoods = get(?ids_ai_food),
    FunFold =
        fun(AiFoodId, {DelFood, FoodAiRecordAcc, MoveChunksAcc}) ->
            AiFood = map_foods:get_food(AiFoodId),
            X = AiFood#ai_food.x,
            Y = AiFood#ai_food.y,
            case map_math:out_c(X, Y, MapR) of
                true ->
                    {[{AiFoodId, AiFood#ai_food.in_chunk} | DelFood], FoodAiRecordAcc, MoveChunksAcc};
                false ->
                    %%获取块，获取块中的玩家，是否在视野内，计算一个在视野内的玩家，计算逃跑角度，计算移动距离
                    ThisFps = AiFood#ai_food.this_fps,
                    ViewChunks = map_math:get_in_chunks(X, Y, AiFood#ai_food.view_r, ChunkWeight),
                    NewSpeed = case config_global:get_v(?ai_food, ?get_ai_food_speed) of
                                   [] -> 240;
                                   Speed ->
                                       Speed
                               end,
                    MoveLen = NewSpeed / ?FPS,
                    Angle = case map_chunks:get_player_chunks(ViewChunks) of
                                [] -> AiFood#ai_food.target_a;
                                AllPlayers -> get_angle(X, Y, AiFood, sets:to_list(AllPlayers))
                            end,
                    {NewX, NewY} = map_math:move(X, Y, Angle, MoveLen),
                    
                    Chunk = map_math:get_xyz(NewX, NewY, ChunkWeight),
                    NewMoveChunks =
                        if
                            AiFood#ai_food.in_chunk =:= Chunk ->
                                MoveChunksAcc;
                            true ->
                                [{AiFoodId, Chunk, AiFood#ai_food.in_chunk} | MoveChunksAcc]
                        end,
                    
                    
                    {DelFood, [AiFood#ai_food{x = NewX, y = NewY, this_fps = ThisFps, in_chunk = Chunk} | FoodAiRecordAcc], NewMoveChunks}
            end
        end,
    {DelIds, Records, MoveChunks} = sets:fold(FunFold, {[], [], []}, AllAiFoods),
    
    del_ai_foods(EtsFoods, DelIds, AllAiFoods),
    update_ets_food(EtsFoods, Records),
    move_chunks(MoveChunks).


refresh_ai_foods() ->
    Sets = get(?ids_ai_food),
    Size = sets:size(Sets),
    
    #{ai_food_view_r := ViewR, ai_food_lv := FoodLv, max_num := MaxNum, model_max_num := ModelMaxNum, refresh_num := RefreshNum} = config_global:get_v(?ai_food),
    
    if
        Size >= RefreshNum -> ok;
        true ->
            Num = erl_random:random(MaxNum - Size),
            FoodAiChunks = get(?chunks_ai_food),
            random_food(ViewR, FoodLv, ModelMaxNum, Num, Sets, FoodAiChunks)
    end.


del_ai_foods(_EtsFoods, [], _AllAiFoods) -> ok;
del_ai_foods(EtsFoods, DelIds, AllAiFoods) ->
    ChunksAiFood = get(?chunks_ai_food),
    ChunksEatFood = get(?chunks_eat_food),
    FunFoldl =
        fun({DelId, Chunk}, {AiIdsAcc, ChunksAiFoodAcc, ChunksEatFoodAcc}) ->
            NewChunksAiFoodAcc =
                case dict:find(Chunk, ChunksAiFoodAcc) of
                    error ->
                        ChunksAiFoodAcc;
                    {ok, Ids} ->
                        dict:store(Chunk, lists:delete(DelId, Ids), ChunksAiFoodAcc)
                end,
            ets:delete(EtsFoods, DelId),
            
            {sets:del_element(DelId, AiIdsAcc), NewChunksAiFoodAcc, map_chunks:append_eat_food_chunks(Chunk, [DelId, 0], ChunksEatFoodAcc)}
        
        end,
    {NewAiIds, NewAiChunks, NewFoodEatChunks} = lists:foldl(FunFoldl, {AllAiFoods, ChunksAiFood, ChunksEatFood}, DelIds),
    put(?ids_ai_food, NewAiIds),
    put(?chunks_ai_food, NewAiChunks),
    put(?chunks_eat_food, NewFoodEatChunks).



update_ets_food(EtsFoods, Records) ->
    ets:insert(EtsFoods, Records).


move_chunks(MoveChunks) ->
    AiAddIds = get(?chunks_ai_food),
    Foldl =
        fun({AiId, NewChunk, OldChunk}, AiAddIdsAcc) ->
            AiAddIdsAcc2 = case dict:find(OldChunk, AiAddIdsAcc) of
                               error ->
                                   AiAddIdsAcc;
                               {ok, Ids} ->
                                   dict:store(OldChunk, lists:delete(AiId, Ids), AiAddIdsAcc)
                           end,
            
            case dict:find(NewChunk, AiAddIdsAcc2) of
                error ->
                    dict:store(NewChunk, [AiId], AiAddIdsAcc2);
                {ok, Ids2} ->
                    dict:store(NewChunk, [AiId | Ids2], AiAddIdsAcc2)
            end
        end,
    put(?chunks_ai_food, lists:foldl(Foldl, AiAddIds, MoveChunks)).


del_ai_food(EtsFoods, Chunk, DelId) when is_integer(DelId) ->
    put(?ids_ai_food, sets:del_element(DelId, get(?ids_ai_food))),
    ets:delete(EtsFoods, DelId),
    map_chunks:remove_ai_food_chunks(DelId, Chunk).


%% 获取逃跑角度
get_angle(X, Y, AiFood, AllPlayers) ->
    case player_in_view(X, Y, AiFood#ai_food.view_r, AllPlayers) of
        {X1, Y1, _Angle} ->
            map_math:xy_to_angle(X1, Y1, X, Y);
        _ -> AiFood#ai_food.target_a
    end.

player_in_view(_X, _Y, _R, []) -> [];

player_in_view(X, Y, R, [PlayerId | Player]) ->
    case map_player_set:get_player(PlayerId) of
        [] -> player_in_view(X, Y, R, Player);
        Player ->
            #obstacle{x = PlayerX, y = PlayerY} = Player#player.head,
            case map_math:p_in_c(PlayerX, PlayerY, X, Y, R) of
                true ->
                    {PlayerX, PlayerY, Player#player.cur_a};
                false ->
                    player_in_view(X, Y, R, Player)
            end
    end.