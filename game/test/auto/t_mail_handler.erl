%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 八月 2016 18:31
%%%-------------------------------------------------------------------
-module(t_mail_handler).
-author("Administrator").

%% API
-include("pub.hrl").
-export([test_accept/2,test_delete/2,test_send/2,test_show/3]).

test_delete(Uid,Id) ->
    case redis_online:is_online(Uid) of
        false ->
            ok;
        {ok, Pid} ->
            ?send_mod_msg(Pid, mail_handler, {delete_mail,Uid,Id})
    end.

test_accept(Uid,Id) ->
    case redis_online:is_online(Uid) of
        false ->
            ok;
        {ok, Pid} ->
            ?send_mod_msg(Pid, mail_handler, {accept_mail,Id})
    end.

test_send(Uid,Id) ->
    case redis_online:is_online(Uid) of
        false ->
            ok;
        {ok, Pid} ->
            ?send_mod_msg(Pid, mail_handler, {send_mail,Id})
    end.

test_show(Uid,Type,Page) ->
    case redis_online:is_online(Uid) of
        false ->
            ok;
        {ok, Pid} ->
            ?send_mod_msg(Pid, mail_handler, {show_mail,Uid,Type,Page})
    end.

















