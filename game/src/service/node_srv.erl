%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc 节点管理，所有游戏服务器，web服务器，连接起来（网管服务器未实现，master监控节点未实现）
%%%
%%% Created : 27. 七月 2016 下午3:06
%%%-------------------------------------------------------------------
-module(node_srv).

-include("pub.hrl").

-behaviour(gen_server).

-define(RPC_TIMEOUT, 5000).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([info/0, send_to_node/4]).

-record(state, {}).

-define(TIMEOUT, 60000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


info() ->
    {ok, Ip} = application:get_env(?snakes, ?ip),
    {ok, Port} = application:get_env(?snakes, ?port),
    {ok, ServerId} = application:get_env(?snakes, ?server_id),
    {ok, ServerName} = application:get_env(?snakes, ?server_name),
    {ok, ServerVersion} = application:get_env(?snakes, ?server_version),
    {ok, Pools} = application:get_env(emysql, pools),
    {log_pool, LogPool} = lists:keyfind(log_pool, 1, Pools),
    {database, LogDB} = lists:keyfind(database, 1, LogPool),
    PlayerCount = player_manager:player_count(),
    MapCount = queue_sup:all_map(),
    {Ip, Port, ServerId, ServerName, ServerVersion, list_to_atom(LogDB), PlayerCount, MapCount}.


init([]) ->
    process_flag(trap_exit, true),
    {ok, Node} = application:get_env(?snakes, ?server_web_node),
%%    net_kernel:monitor_nodes(true),
    case net_kernel:connect_node(Node) of
        true ->
            {node_master, Node} ! {nodeup, node(), info()};
        false ->
            ?DEBUG("server_web close wait 60s reconnect:~p~n", [Node])
    end,
    erlang:start_timer(?TIMEOUT, self(), ?reconnect),
    {ok, #state{}}.



handle_call({PidStr, Msg}, _From, State) ->
    PlayerPid = list_to_pid(binary_to_list(PidStr)),
        catch gen_server:call(PlayerPid, Msg),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({timeout, _TimerRef, ?reconnect}, State) ->
    {ok, Node} = application:get_env(?snakes, ?server_web_node),
    case net_kernel:connect_node(Node) of
        true ->
            {node_master, Node} ! {nodeup, node(), info()};
        false ->
            ?DEBUG("server_web close wait 10s reconnect:~p~n", [Node])
    end,
    erlang:start_timer(?TIMEOUT, self(), ?reconnect),
    {noreply, State};

handle_info({MyId, MyPidStr, FromPidStr, FromNode, Msg}, State) ->
    PlayerPid = list_to_pid(binary_to_list(MyPidStr)),
    case erlang:is_process_alive(PlayerPid) of
        true -> PlayerPid ! Msg;
        false ->
            redis_online:del(MyId),
            send_to_node(0, FromPidStr, FromNode, {error, ?ERR_NOT_ONLINE})
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    {ok, Node} = application:get_env(?snakes, ?server_web_node),
    case net_kernel:connect_node(Node) of
        true ->
            {node_master, Node} ! {nodedown, node()};
        false ->
            ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


send_to_node(ToId, ToPidStr, ToNode, Msg) ->
    {?MODULE, ToNode} ! {ToId, ToPidStr, list_to_binary(pid_to_list(self())), node(), Msg}.
