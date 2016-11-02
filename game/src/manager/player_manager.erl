%%%
%%% @doc 玩家在此记录,
%%% @todo 以渠道来区分在线玩家，方便跑马灯按照渠道来推送
%%% Created : 15. 十二月 2015 下午2:45
%%%-------------------------------------------------------------------

-module(player_manager).

-behaviour(gen_server).

-include("pub.hrl").

-export([add/2, del/2, player_count/0, abcast/2, abcast/3, select_player_ids/0]).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(ETS_MANAGER_PLAYER, manager_player).
-define(ETS_MANAGER_PLAYER_PIDS, manager_player_pids).

-define(all_player_pid, all_player_pid).

-record(state, {}).

-define(timeout_tick_time, 300000).  %多少时间数据持久化
-define(timeout_tick_save_data, timeout_tick_save_data).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% 添加一个玩家
add(Pid, PlayerId) ->
    gen_server:cast(?MODULE, {add, Pid, PlayerId}),
    redis_online:set(PlayerId, Pid, node()).

%% 删除一个玩家
del(Pid, PlayerId) ->
    gen_server:cast(?MODULE, {del, Pid, PlayerId}),
    redis_online:del(PlayerId).


is_online(PlayerId) ->
    gen_server:call(?MODULE, {is_online, PlayerId}).


%% 本台服务器上的玩家
abcast(Mod, Arg) ->
    gen_server:cast(?MODULE, {abcast, Mod, Arg}).


%% 本台服务器上的玩家
abcast(Mod, Arg, Uid) when is_integer(Uid) ->
    case is_online(Uid) of
        {ok, Pid} -> ?send_mod_msg(Pid, Mod, Arg);
        false -> false
    end;

abcast(Mod, Arg, Uids) when is_list(Uids) ->
    Fun =
        fun(Uid) ->
            case is_online(Uid) of
                {ok, Pid} -> ?send_mod_msg(Pid, Mod, Arg);
                false -> false
            end
        end,
    lists:map(Fun, Uids).


%% 获得当前玩家总数
player_count() ->
    ets:info(?ETS_MANAGER_PLAYER, size).


select_player_ids() ->
    Ids = gen_server:call(?MODULE, select_player_ids),
    Len = length(Ids),
    R1 = erl_random:random(Len),
    R2 = erl_random:random(Len),
    R3 = erl_random:random(Len),
    R4 = erl_random:random(Len),
    R5 = erl_random:random(Len),
    [lists:nth(I, Ids) || I <- lists:usort([R1, R2, R3, R4, R5])].


init([]) ->
    ets:new(?ETS_MANAGER_PLAYER, [named_table, {read_concurrency, true}]),
    ets:new(?ETS_MANAGER_PLAYER_PIDS, [named_table, {read_concurrency, true}]),
    ?INFO("~p init done~n", [?MODULE]),
    
    erlang:start_timer(?timeout_tick_time, self(), ?timeout_tick_save_data),
    {ok, #state{}}.


handle_call({is_online, PlayerId}, _From, State) ->
    Reply = case ets:lookup(?ETS_MANAGER_PLAYER, PlayerId) of
                [] -> false;
                [{PlayerId, Pid}] ->
                    {ok, Pid}
            end,
    {reply, Reply, State};

handle_call(select_player_ids, _From, State) ->
    
    Reply = case ets:select(?ETS_MANAGER_PLAYER, [{{'$1', '$2'}, [], ['$1']}], 100) of
                '$end_of_table' -> [];
                {Ids, _} -> Ids
            end,
    
    {reply, Reply, State}.


handle_cast({abcast, Mod, Arg}, State) ->
    case ets:lookup(?ETS_MANAGER_PLAYER_PIDS, ?all_player_pid) of
        [] -> ok;
        [{?all_player_pid, Sets}] ->
            sets:fold(
                fun(Pid, Acc) ->
                    case is_process_alive(Pid) of
                        true ->
                            ?send_mod_msg(Pid, Mod, Arg);
                        false ->
                            ok
                    end,
                    Acc
                end,
                [],
                Sets)
    end,
    {noreply, State};

handle_cast({add, Pid, PlayerId}, State) ->
    S2 = case ets:lookup(?ETS_MANAGER_PLAYER_PIDS, ?all_player_pid) of
             [] ->
                 sets:add_element(Pid, sets:new());
             [{?all_player_pid, S1}] ->
                 sets:add_element(Pid, S1)
         end,
    true = ets:insert(?ETS_MANAGER_PLAYER_PIDS, {Pid, PlayerId}),
    true = ets:insert(?ETS_MANAGER_PLAYER, {PlayerId, Pid}),
    ets:insert(?ETS_MANAGER_PLAYER_PIDS, {?all_player_pid, S2}),
    {noreply, State};

handle_cast({del, Pid, PlayerId}, State) ->
    case ets:member(?ETS_MANAGER_PLAYER, PlayerId) of
        true ->
            NewSets = case ets:lookup(?ETS_MANAGER_PLAYER_PIDS, ?all_player_pid) of
                          [] -> sets:new();
                          [{?all_player_pid, Sets}] ->
                              sets:del_element(Pid, Sets)
                      end,
            ets:insert(?ETS_MANAGER_PLAYER_PIDS, {?all_player_pid, NewSets}),
            ets:match_delete(?ETS_MANAGER_PLAYER_PIDS, {'_', PlayerId}),
            true = ets:delete(?ETS_MANAGER_PLAYER, PlayerId);
        false ->
            ok
    end,
    {noreply, State}.


handle_info({timeout, _TimerRef, ?timeout_tick_save_data}, State) ->
    {ok, ServerId} = application:get_env(?snakes, ?server_id),
        catch log_mod:online_m(ets:info(?ETS_MANAGER_PLAYER, size), ServerId),
    erlang:start_timer(?timeout_tick_time, self(), ?timeout_tick_save_data),
    {noreply, State};

handle_info(_Error, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

