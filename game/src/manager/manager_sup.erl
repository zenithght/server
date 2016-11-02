%%%-------------------------------------------------------------------
%%% @author yujian
%%% @doc
%%% Created : 15. 十二月 2015 下午2:45
%%%-------------------------------------------------------------------
-module(manager_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Child = [
        ?CHILD(player_manager, worker),
        ?CHILD(activity_manager, worker)
    ],
    {ok, {{one_for_one, 5, 10}, Child}}.
