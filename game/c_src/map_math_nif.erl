%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%%
%%% Created : 27. 六月 2016 下午3:20
%%%-------------------------------------------------------------------
-module(map_math_nif).

%%-on_load(init/0).

-export([get_xyz/3, get_in_chunks/4, map_chunks/2, view_chunks/4, move_body/8]).


%%init() ->
%%    Path = case code:lib_dir(leofs_extra, priv) of
%%               {error, _} -> "./priv/map_math_nif";
%%               Str -> Str ++ "/map_math_nif"
%%           end,
%%
%%    erlang:load_nif(Path, 0).


-spec get_in_chunks(float(), float(), integer(), integer()) -> list().
%% @doc 坐标在块内
get_in_chunks(_X, _Y, _R, _ChunkWeight) ->
    "NIF error".


%% @doc 原始坐标转换 (-1,-1) -> {-1, -1} (0, 0) -> {1, 1}
-spec get_xyz(float(), float(), integer()) -> {integer(), integer()}.
get_xyz(_X, _Y, _ChunkWeight) ->
    "NIF error".


-spec map_chunks(integer(), integer()) -> list().
map_chunks(_MapR, _ChunkWeight) ->
    "NIF error".



-spec view_chunks(float(), float(), integer(), integer()) -> list().
view_chunks(_HeadX, _HeadY, _MapR, _ChunkWeight) ->
    "NIF error".

move_body(_OldX, _OldY, _Xa, _Ya, _Xb, _Yb, _Gap, _Speed) ->
    "NIF error".