%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 九月 2016 16:42
%%%-------------------------------------------------------------------
-module(t_sign_handler).
-author("Administrator").

%% API
-include("pub.hrl").
-export([t_sign/2,t_sign_fill_up/2,t_show/1]).

t_sign(Uid,Id) ->
    case redis_online:is_online(Uid) of
        false ->
            ok;
        {ok, Pid} ->
            ?send_mod_msg(Pid, sign_handler, {sign,Id})
    end.

t_sign_fill_up(Uid,N) ->
    case redis_online:is_online(Uid) of
        false ->
            ok;
        {ok, Pid} ->
            ?send_mod_msg(Pid, sign_handler, {sign_fill_up,N})
    end.

t_show(Uid) ->
    case redis_online:is_online(Uid) of
        false ->
            ok;
        {ok, Pid} ->
            ?send_mod_msg(Pid, sign_handler, {show,Uid})
    end.

