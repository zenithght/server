-define(send_to_client, send_to_client).


-define(mod_msg(Mod, Msg), {mod, Mod, self(), ?MODULE, Msg}).
-define(send_mod_msg(Pid, Mod, Msg), Pid ! {mod, Mod, self(), ?MODULE, Msg}).

-define(remote_mod_msg(Mod, Msg), {mod, Mod, pid_to_list(self()), ?MODULE, Msg}).

-define(to_mod_msg(Uid, Mod, Msg),
    case redis_online:is_online(Uid) of
        {ok, Pid} -> ?send_mod_msg(Pid, Mod, Msg);
        {ok, Node, Pid} -> {node_srv, Node} ! {Pid, ?remote_mod_msg(Mod, Msg)};
        false -> false
    end).

-define(start_timer(Time, Mod, Msg), erlang:start_timer(Time, self(), {mod, Mod, self(), ?MODULE, Msg})).

-define(send_to_client(Pid, Msg), Pid ! {?send_to_client, Msg}).
-define(to_client_msg(Msg), {?send_to_client, Msg}).


-define(tcp_send(State, Data), player_server:send(State, Data), State).
