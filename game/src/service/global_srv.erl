%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%%
%%% Created : 17. 八月 2016 下午3:18
%%%-------------------------------------------------------------------
-module(global_srv).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
    insert/2,
    lookup/1
]).

-record(state, {}).

-define(SERVICE_GLOBAL, service_global).

insert(K, V) ->
    ets:insert(?SERVICE_GLOBAL, {K, V}).

lookup(K) ->
    ets:lookup(?SERVICE_GLOBAL, K).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    ets:new(?SERVICE_GLOBAL, [named_table, public, {read_concurrency, true}]),
    {ok, #state{}}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
