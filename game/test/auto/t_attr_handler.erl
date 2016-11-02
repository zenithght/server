%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 八月 2016 18:31
%%%-------------------------------------------------------------------
-module(t_attr_handler).
-author("Administrator").

%% API
-include("pub.hrl").
-export([test_model/2,test_nick/2]).

test_model(Uid,Int) ->
    case player_manager:is_online(Uid) of
        false ->
            ok;
        {ok, Pid} ->
            ?send_mod_msg(Pid, attr_handler, {model,Int})
    end.

test_nick(Uid,String) ->
    case player_manager:is_online(Uid) of
        false ->
            ok;
        {ok, Pid} ->
            ?send_mod_msg(Pid, attr_handler, {nick,String})
    end.
















