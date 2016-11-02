%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%%
%%% Created : 02. 八月 2016 上午10:48
%%%-------------------------------------------------------------------
-module(t_login_handler).

-include("pub.hrl").

-compile(export_all).

-export([handler/1, send/2]).

-define(SERVER_HTTP_IP, <<"http://127.0.0.1:8080/login/">>).


login() ->
    Time = integer_to_binary(erl_time:now()),
    Sign = list_to_binary(erl_hash:md5(<<"date=", Time/binary, "&key=123">>)),
    
    Url = binary_to_list(<<?SERVER_HTTP_IP/binary, "account/?date=", Time/binary, "&sign=", Sign/binary, "&account_name=1&account_pwd=1">>),
    {ok, {_, _, R2}} = httpc:request(get, {Url, []}, [], []),
    case R2 of
        "-1" -> io:format("~p~n", ["password error"]);
        R2 ->
            {Data} = jiffy:decode(R2),
            Uid = proplists:get_value(<<"uid">>, Data),
            Token = proplists:get_value(<<"token">>, Data),
            {Uid, Token}
    end.




handler([?LOGIN_HANDLER, ?PROTO_LOG_LOGIN, 1]) ->
    put(validity, 2);

handler(_Data) ->
    ?PRINT("err no this proto:~p~n", [_Data]).


send(?PROTO_LOGIN, {}) ->
    {Uid, Token} = login(),
    [?LOGIN_HANDLER, [?PROTO_LOGIN, Token, Uid]].
