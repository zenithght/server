%%% -------------------------------------------------------------------
%%% Author  : Administrator
%%% Description :
%%%
%%% Created : 2012-9-24
%%% -------------------------------------------------------------------
-module(player_server).

-include("pub.hrl").

-export([send/2, pack_encode/1, pack_encode/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(PROCESS_TICK, process_tick).%心跳计时
-define(PROCESS_TICK_TIME, 60000).    %心跳间隔时间

init({Socket}) ->
%%	io:format("start player process:~p~n", [ [self(), Socket] ]),
    erlang:start_timer(?PROCESS_TICK_TIME, self(), ?PROCESS_TICK),
    
    {ok, #player_state{socket = Socket}}.

handle_call({stop, ErrCode}, _From, State) ->
    send(State, [0, 0, ErrCode]),
    {stop, normal, State};

handle_call({call, M, F, A}, _From, State) ->
    {BackA, State1} = M:F(State, A),
    {reply, BackA, State1};

handle_call({call, M, F}, _From, State) ->
    {BackA, State1} = M:F(State),
    {reply, BackA, State1}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({mod, Mod, From, FromModule, Msg}, State) ->
    NewState =
        if
            State#player_state.login_state =:= ?LOGIN_INIT_DONE ->
                case catch Mod:handler_msg(State, From, FromModule, Msg) of
                    {throw, ErrCode} ->
                        err_code_handler:err_code(State, {mod, Mod, From, FromModule, Msg}, {throw, ErrCode});
                    State -> State
                end;
            true ->
                State
        end,
    {noreply, NewState};

handle_info({error, Err}, State) ->
    err_code_handler:err_code(State, 0, {throw, Err}),
    {noreply, State};

handle_info({tcp, _Socket, RecvBin}, State) ->
%%    ?LOG("RecvBin:~p...~n", [RecvBin]),
    
    case catch ?proto_decode(RecvBin) of
        [Validity, ModId, Json] ->
%%
            case catch login_handler_can:validity_check(Validity) of
                ok ->
                    {noreply, recvPack(ModId, Json, State)};
                _Check ->
                    {stop, normal, State}
            end;
        _ ->
            {stop, normal, State}
    end;


%% 发数据包
handle_info({?send_to_client, Msg}, State) ->
    send(State, Msg),
    {noreply, State};


%% 发数据包
handle_info(stop, State) ->
    {stop, normal, State};


handle_info({inet_reply, _Sock, _Error}, State) ->
    if
        _Error =:= ok -> ok;
        true ->
            ?ERROR("inet reply error: ~p~n", [_Error])
    end,
    {noreply, State};


handle_info({timeout, _TimerRef, ?PROCESS_TICK}, State) ->
%%    io:format("tcp timeout tick:~n"),
    erlang:start_timer(?PROCESS_TICK_TIME, self(), ?PROCESS_TICK),
    case State#player_state.tick of
        0 ->
            {stop, normal, State};
        _ ->
            {noreply, State#player_state{tick = 0}}
    end;

handle_info({timeout, _TimerRef, {mod, Mod, From, FromModule, Msg}}, State) ->
%%    io:format("timer callback:~p~n", [{mod, Mod, From, FromModule, Msg}]),
    NewState =
        if
            State#player_state.login_state =:= ?LOGIN_INIT_DONE ->
                case catch Mod:handler_msg(State, From, FromModule, Msg) of
                    {throw, ErrCode} ->
                        err_code_handler:err_code(State, {mod, Mod, From, FromModule, Msg}, {throw, ErrCode});
                    State -> State
                end;
            true ->
                State
        end,
    {noreply, NewState};

handle_info(_Info, State) ->
%%    ?WARN("handle_info: ~w~nState:~p~n", [_Info, State]),
    {noreply, State}.


terminate(_Reason, #player_state{uid = _Uid} = State) ->
%%    ?DEBUG("player process terminate:~p~n", [[_Reason, _Uid, self()]]),
    
    if
        State#player_state.login_state =:= ?LOGIN_INIT_DONE ->
            player_behaviour:terminate(State#player_state{login_state = ?PLAYER_TERMINATE}),
            player_manager:del(self(), _Uid);
        true ->
            ok
    end,
    %% @doc 登陆打点，可能存在无法登陆的情况
    log_mod:log_login_save().


recvPack(ModId, [ProtoId | Data], State) ->
%%    ?WARN("111~p~n", [[ModId, [ProtoId | Data]]]),
    Ret = if
              State#player_state.login_state =:= ?LOGIN_INIT_DONE ->
                  {Mod, _ProtoMod} = all_proto:lookup_cmd(ModId),
                  if
                      Mod =:= error ->
                          ?DEBUG("error no module_id:~p~n", [ModId]),
                          State;
                      Mod =:= ?login_handler andalso ProtoId =:= ?PROTO_LOGIN ->
                          ?DEBUG("error relogin state:~p~n", [State]),
                          State;
                      Mod =:= ?login_handler andalso ProtoId =:= ?PROTO_MOVE_SERVER_LOGIN ->
                          ?DEBUG("error relogin state:~p~n", [State]),
                          State;
                      true ->
%%            Arg = ProtoMod:decode(ProtoId, Data),
                          ?put_new(?proto_id, {ModId, ProtoId}),
                              catch Mod:handle_info(State#player_state{tick = State#player_state.tick + 1}, ProtoId, Data)
                  end;
              true ->
%%                          ?DEBUG("not login:~p~n", [[ModId, ProtoId | Data]]),
                  State
          end,
    if
        is_record(Ret, player_state) -> Ret;
        true ->
            err_code_handler:err_code(State, Ret, ModId, ProtoId, Data),
            State
    
    end.


% 打包json数据
pack_encode(Pack) ->
    ls_proto_ws:pack_encode(?encode(Pack)).

% 打包二进制数据
pack_encode(PackBin, 2) ->
    ls_proto_ws:pack_encode(PackBin, 2).

send(Port, Bin) when is_port(Port) ->
        catch erlang:port_command(Port, Bin, [force]);

send(State, Bin) when is_record(State, player_state) andalso is_binary(Bin) ->
        catch erlang:port_command(State#player_state.socket, Bin, [force]);

send(State, Pack) when is_record(State, player_state) ->
        catch erlang:port_command(State#player_state.socket, pack_encode(Pack), [force]);

send(_Socket, _Bin) -> ok.