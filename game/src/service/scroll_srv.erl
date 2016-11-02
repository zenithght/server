%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc 跑马灯， 每5分总读取一次数据库，刷新缓存，同时启动diff 倒计时，倒计时时间后调用manager_player 发送跑马灯信息给客户端
%%%
%%% Created : 29. 七月 2016 下午3:59
%%%-------------------------------------------------------------------
-module(scroll_srv).

-include("pub.hrl").

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([select_id/1]).

-record(state, {}).

-define(ETS_SERVICE_SCROLL, service_scroll).
-define(tick_time, 300000). %5分钟刷新一次

-record(notice_scroll, {
    id,
    server_id,
    stime,
    etime,
    diff_time,
    content
}).


select_id(Id) ->
    case ets:lookup(?ETS_SERVICE_SCROLL, Id) of
        [] -> [];
        [Record] ->
            Record#notice_scroll.content
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    ets:new(?ETS_SERVICE_SCROLL, [named_table, {keypos, #notice_scroll.id}, {read_concurrency, true}]),
    
    refresh_time(),
    erlang:start_timer(?tick_time, self(), timeout_tick),
    {ok, #state{}}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({timeout, _TimerRef, {abcast_tick, Id}}, State) ->
    case ets:lookup(?ETS_SERVICE_SCROLL, Id) of
        [] -> ok;
        [#notice_scroll{diff_time = DiffTime}] ->
            player_manager:abcast(?chat_handler, {abcast, Id}),
            erlang:start_timer(DiffTime * 1000, self(), {abcast_tick, Id})
    end,
    {noreply, State};

handle_info({timeout, _TimerRef, timeout_tick}, State) ->
        catch refresh_time(),
    erlang:start_timer(?tick_time, self(), timeout_tick),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


refresh_time() ->
    OldIds = case ets:lookup(?ETS_SERVICE_SCROLL, all_ids) of
                 [] ->
                     [];
                 [{_, _, IdList1}] ->
                     IdList1
             end,
    
    Data = load_notice_scroll:select_scroll(),
    FunFoldl =
        fun([Id, ServerId, STime, ETime, DiffTime, Content], {Ids, ItemAcc}) ->
            ItemAcc2 =
                if
                    is_integer(DiffTime) andalso DiffTime >= 120 ->
                        [#notice_scroll{id = Id, server_id = ServerId, stime = STime, etime = ETime, diff_time = DiffTime, content = Content} | ItemAcc];
                    true ->
                        io:format("config diff_time need >= 120s~n"),
                        ItemAcc
                end,
            
            {[Id | Ids], ItemAcc2}
        end,
    {NewIds, Items} = lists:foldl(FunFoldl, {[], []}, Data),
    
    DelIds = erl_list:diff(OldIds, NewIds, []),
    AddIds = erl_list:diff(NewIds, OldIds, []),
    
    [ets:delete(?ETS_SERVICE_SCROLL, Id) || Id <- DelIds],
    ets:insert(?ETS_SERVICE_SCROLL, [{notice_scroll, all_ids, NewIds} | Items]),
    
    [erlang:start_timer(DiffTime * 1000, self(), {abcast_tick, Id}) || #notice_scroll{id = Id, diff_time = DiffTime} <- Items, lists:member(Id, AddIds)].
    
    
    
    
    