%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc 活动管理器
%%%
%%% Created : 25. 八月 2016 下午5:15
%%%-------------------------------------------------------------------
-module(activity_manager).

-include("pub.hrl").

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {t_ref}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    All = config_activity:all(),
    lists:map(fun(R) -> start_timer(R) end, All),
    
    {ok, #state{}}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({timeout, _TimerRef, {refresh, Id}}, State) ->
    ?DEBUG("111:~p~n", [{refresh, Id}]),
    case config_activity:diff_time(Id) of
        {once, _Time} -> ok;
        {cycle, DiffTime} ->
            case Id of
                ?player_refresh_time -> player_manager:abcast(?attr_handler, {?event_zero_refresh});
                ?arena_refresh_time -> arena_manager:arena_refresh(erl_time:now());
                _ -> ok
            end,
            erlang:start_timer(DiffTime * 1000, self(), {refresh, Id})
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


start_timer({Id, W, Month, D, H, M, _PushC}) ->
    {_Type, DiffTime} = config_activity:diff_time(W, Month, D, H, M),
    if
        DiffTime < 0 -> ok;
        true -> erlang:start_timer(DiffTime * 1000, self(), {refresh, Id})
    end.

