-module(snakes_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(CHILD(I, Type, Arg), {I, {I, start_link, Arg}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    Child = [
        ?CHILD(map_server_sup, supervisor, []),
        ?CHILD(ai_sup, supervisor, []),
        ?CHILD(manager_sup, supervisor, []),
        ?CHILD(service_sup, supervisor, [])
    ],
    
    {ok, {{one_for_one, 5, 10}, Child}}.

