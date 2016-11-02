%%%-------------------------------------------------------------------
%%% @author yujian
%%% @doc
%%% Created : 15. 十二月 2015 下午2:45
%%%-------------------------------------------------------------------
-module(log_sup).

-behaviour(supervisor).

-include("pub.hrl").

-export([start_link/0, init/1]).

-export([
    add_attr_log/4,
    add_item_log/4,
    add_skin_log/4,
    
    add_log_arena_fight/9
]).

-define(CHILD(Name, I, Type, Arg), {Name, {I, start_link, [Arg]}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    AttrIds = config_attr:all_id(),
    ItemIds = config_item:all_id(),
    SkinIds = config_skin:all_id(),
    
    LogAllIds = [to_skin_pid(Id) || Id <- SkinIds] ++ [to_attr_pid(Id) || Id <- AttrIds] ++ [to_item_pid(Id) || Id <- ItemIds],
    AllIds = [
%%        ?log_connect,
        ?log_arena_fight
        | LogAllIds
    ],
    
    
    Child = [?CHILD(Id, log_srv, worker, Id) || Id <- AllIds],
    
    {ok, {{one_for_one, 5, 10}, Child}}.


%% Type = add||del.
add_attr_log(PlayerId, Type, TypeId, KvList) ->
    lists:map(
        fun({ItemId, ItemV}) ->
            PidName = to_attr_pid(ItemId),
            case whereis(PidName) of
                undefined ->
                    ok;
                _ ->
                    gen_server:cast(PidName, {add_log, PlayerId, Type, TypeId, ItemV})
            end
        end,
        KvList).

add_item_log(PlayerId, Type, TypeId, KvList) ->
    lists:map(
        fun({ItemId, ItemV}) ->
            PidName = to_item_pid(ItemId),
            case whereis(PidName) of
                undefined ->
                    ok;
                _ ->
                    gen_server:cast(PidName, {add_log, PlayerId, Type, TypeId, ItemV})
            end
        end,
        KvList).

add_skin_log(PlayerId, Type, TypeId, KvList) ->
    lists:map(
        fun({ItemId, ItemV}) ->
            PidName = to_skin_pid(ItemId),
            case whereis(PidName) of
                undefined ->
                    ok;
                _ ->
                    gen_server:cast(PidName, {add_log, PlayerId, Type, TypeId, ItemV})
            end
        end,
        KvList).


add_log_arena_fight(UUid, PlayerId, STime, ETime, KillNum, KilledNum, MaxScore, IsWin, IsLeave) ->
    gen_server:cast(?log_arena_fight, {add_log_arena_fight,
        list_to_binary(UUid),
        integer_to_binary(PlayerId),
        integer_to_binary(STime),
        integer_to_binary(ETime),
        integer_to_binary(KillNum),
        integer_to_binary(KilledNum),
        integer_to_binary(MaxScore),
        integer_to_binary(IsWin),
        integer_to_binary(IsLeave)}).


to_attr_pid(K) -> list_to_atom("log_attr_id_" ++ integer_to_list(K)).

to_item_pid(K) -> list_to_atom("log_item_id_" ++ integer_to_list(K)).

to_skin_pid(K) -> list_to_atom("log_skin_id_" ++ integer_to_list(K)).