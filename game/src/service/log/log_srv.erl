%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%%
%%% Created : 22. 七月 2016 下午5:19
%%%-------------------------------------------------------------------
-module(log_srv).

-include("pub.hrl").
-include("log_srv.hrl").
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(state, {type, log = [], count = 0}).

-define(TIMEOUT, 300000).
-define(tick_save_data, tick_save_data).


start_link(LogName) ->
    gen_server:start_link({local, LogName}, ?MODULE, LogName, []).


init(LogName) ->
    process_flag(trap_exit, true),
    
    erlang:start_timer(?TIMEOUT, self(), ?tick_save_data),
    
    {ok, #state{type = LogName}}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};


%% @doc 资产日志
handle_cast({add_log, PlayerId, Type, TypeId, Num}, State) ->
    Now = integer_to_binary(erl_time:now()),
    NewState = if
                   State#state.count + 1 =:= 1000 ->
                       save_data(State#state.type, State#state.log),
                       State#state{log = [{PlayerId, Type, TypeId, Num, Now}], count = 1};
                   true ->
                       State#state{log = [{PlayerId, Type, TypeId, Num, Now} | State#state.log], count = State#state.count + 1}
               end,
    {noreply, NewState};

%% @doc 启动次数
handle_cast({add_log_arena_fight, UUid, PlayerId, STime, ETime, KillNum, KilledNum, MaxScore, IsWin, IsLeave}, State) ->
    NewState = if
                   State#state.count + 1 =:= 1000 ->
                       save_data_arena_fight(State#state.log),
                       State#state{log = [{UUid, PlayerId, STime, ETime, KillNum, KilledNum, MaxScore, IsWin, IsLeave}], count = 1};
                   true ->
                       State#state{log = [{UUid, PlayerId, STime, ETime, KillNum, KilledNum, MaxScore, IsWin, IsLeave} | State#state.log], count = State#state.count + 1}
               end,
    {noreply, NewState};


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({timeout, _TimerRef, ?tick_save_data}, State) ->
    case State#state.type of
        ?log_arena_fight ->
            save_data_arena_fight(State#state.log);
        _ ->
            save_data(State#state.type, State#state.log)
    end,
    erlang:start_timer(?TIMEOUT, self(), ?tick_save_data),
    {noreply, State#state{log = [], count = 0}};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, State) ->
    case State#state.type of
        ?log_arena_fight ->
            save_data_arena_fight(State#state.log);
        _ ->
            save_data(State#state.type, State#state.log)
    end.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

save_data(LogType, Data) ->
    Tab = list_to_binary(atom_to_list(LogType)),
    Sql = lists:map(
        fun({PlayerId, Type, TypeId, Num, Now}) ->
            NewNum = if
                         is_binary(Num) -> Num;
                         is_integer(Num) -> integer_to_binary(Num)
                     end,
            ?LOG_ATTR_ITEM(Tab, PlayerId, Type, TypeId, NewNum, Now)
        end,
        Data),
    
    case Sql of
        [] -> ok;
        Sql1 ->
                catch erl_mysql:el(Sql1)
    end.

save_data_arena_fight(Data) ->
    Sql = lists:map(fun({UUid, PlayerId, STime, ETime, KillNum, KilledNum, MaxScore, IsWin, IsLeave}) ->
        ?LOG_ARENA_FIGHT(UUid, PlayerId, STime, ETime, KillNum, KilledNum, MaxScore, IsWin, IsLeave)
                    end,
        Data),
    case Sql of
        [] -> ok;
        Sql1 ->
                catch erl_mysql:el(Sql1)
    end.