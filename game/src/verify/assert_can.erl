%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%%
%%% Created : 18. 七月 2016 上午9:17
%%%-------------------------------------------------------------------
-module(assert_can).

-include("pub.hrl").

-export([
    natural_num/1,
    
    exit_dict/1,
    exit_process/1,
    exit_process/2,
    
    verify_illegal/1,
    verify_mask_word/1
]).

%% @doc 自然数（0,正整数）
natural_num(Int) ->
    case is_integer(Int) of
        true ->
            case Int >= 0 of
                true -> ok;
                false -> ?return_err(?ERR_NOT_NATURAL_NUM)
            end;
        false ->
            ?return_err(?ERR_NOT_INTEGER)
    end.

exit_dict(Key) ->
    exit_dict(Key, true).

exit_dict(Key, true) ->
    case erlang:get(Key) of
        undefined -> ?return_err(?ERR_NO_PROCESS_DICT);
        V -> V
    end;

exit_dict(Key, false) ->
    case erlang:get(Key) of
        undefined -> ok;
        _V -> ?return_err(?ERR_EXIT_PROCESS_DICT)
    end.


exit_process(Pid) ->
    exit_process(Pid, true).

exit_process(Pid, true) ->
    case is_pid(Pid) of
        true ->
            case is_process_alive(Pid) of
                true -> ok;
                false -> ?return_err(?ERR_NO_PROCESS)
            end
    end;

exit_process(Pid, false) ->
    case is_pid(Pid) of
        true ->
            case is_process_alive(Pid) of
                false -> ok;
                true -> ?return_err(?ERR_EXIT_PROCESS)
            end
    end.


verify_illegal(Binary) ->
    case erl_mysql:illegal_character(Binary) of
        true ->
            ok;
        false ->
            ?return_err(?ERR_ILLEGAL_CHATS)
    end.

verify_mask_word(Binary) ->
    case cpn_mask_word:checkRes(Binary) of
        [_, false] -> ok;
        [_, true] -> ?return_err(?ERR_ATTR_SENSITIVE_CHARACTER)
    end.