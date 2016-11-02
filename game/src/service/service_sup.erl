%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%%
%%% Created : 03. 八月 2016 上午9:42
%%%-------------------------------------------------------------------
-module(service_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1, start_child/0]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Child = [
        ?CHILD(global_srv, worker),
        ?CHILD(netease_im_srv, worker),
        ?CHILD(log_sup, supervisor),
        ?CHILD(node_srv, worker),
        ?CHILD(scroll_srv, worker)
%%        ?CHILD(sms_srv, worker)
    
    ],
    {ok, {{one_for_one, 5, 10}, Child}}.

start_child() ->
    application:set_env(snakes, server_web_node, 'snakes_web@120.27.139.235'),
    supervisor:start_child(?MODULE, ?CHILD(node_srv, worker)).