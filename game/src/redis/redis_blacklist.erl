%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc redis 操作
%%%
%%% Created : 14. 九月 2016 下午3:56
%%%-------------------------------------------------------------------
-module(redis_blacklist).

-include("pub.hrl").
-include("erl_redis.hrl").

-export([exit/1, set/2, in_my_black/2]).

exit(MyUid) ->
    case eredis_pool:q(pool, [<<"EXISTS">>, ?TAB_BLACKLIST(MyUid)]) of
        {ok, <<"1">>} -> ok;
        _ -> load_friend_blacklist:load_data_in_redis(MyUid)
    end.


set(UidBin, Arg) ->
    eredis_pool:qp(pool, [
        [<<"HMSET">>, ?TAB_BLACKLIST(UidBin) | Arg],
        [<<"EXPIRE">>, ?TAB_BLACKLIST(UidBin), ?EXPIRE_TIME_2]
    ]).


in_my_black(Uid, BlackUid) ->
    UidBin = integer_to_binary(Uid),
    case eredis_pool:q(pool, [<<"HMGET">>, ?TAB_BLACKLIST(UidBin), BlackUid]) of
        {ok, ?undefined} -> ?false;
        {ok, _} -> ?true;
        _ -> ?false
    end.
