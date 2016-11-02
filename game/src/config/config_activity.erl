%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%%
%%% Created : 27. 七月 2016 下午7:14
%%%-------------------------------------------------------------------
-module(config_activity).

-include_lib("config/include/config_mate.hrl").
-include("pub.hrl").

-export([
    all/0,
    get_v/1,
    now_refresh_times/1,
    diff_time/1,
    diff_time/5
]).

-define(tab_name, config_activity).

-record(config_activity, {
    id,
    week,
    month,
    day,
    hour,
    minite,
    comment,
    push_c
}).

load_config() ->
    [
        #config_mate{
            name = ?tab_name,
            record = #config_activity{},
            type = mysql,
            mysql_pool = static_pool,
            fields = record_info(fields, ?tab_name),
            all = [#config_activity.id],
            group = [],
            rewrite = fun rewrite/1,
            verify = fun verify/1
        }
    ].

rewrite(Item = #config_activity{week = W, month = Month, day = D, hour = H, minite = M}) ->
    Fun = fun(X) ->
        case X of
            <<"*">> -> <<"*">>;
            X -> binary_to_integer(X)
        end
          end,
    Item#config_activity{week = Fun(W), month = Fun(Month), day = Fun(D), hour = Fun(H), minite = Fun(M)}.


verify(#config_activity{id = Id, week = W, month = Month, day = D, hour = H, minite = M, push_c = PushC}) ->
    ?check((is_integer(W) andalso W >= 1 andalso W =< 7) orelse (W =:= <<"*">>), "config_activity id:~p week:~p error~n", [Id, W]),
    ?check((is_integer(Month) andalso Month >= 1 andalso Month =< 31) orelse (Month =:= <<"*">>), "config_activity id:~p Month:~p error~n", [Id, Month]),
    ?check((is_integer(D) andalso D >= 1 andalso D =< 30) orelse (D =:= <<"*">>), "config_activity id:~p D:~p error~n", [Id, D]),
    ?check((is_integer(H) andalso H >= 0 andalso H =< 24) orelse (H =:= <<"*">>), "config_activity id:~p H:~p error~n", [Id, H]),
    ?check((is_integer(M) andalso M >= 0 andalso M =< 60) orelse (M =:= <<"*">>), "config_activity id:~p M:~p error~n", [Id, M]),
    ?check(is_binary(PushC) orelse PushC =:= ?undefined, "config_activity id:~p PushC:~p error~n", [Id, PushC]),
    true.


all() ->
    [{_, _, Ids}] = ets:lookup(?tab_name, {all, #config_activity.id}),
    FunFoldl =
        fun(Id, Acc) ->
            if
                Id =< 10 -> Acc;
                true ->
                    [R] = ets:lookup(?tab_name, Id),
                    [
                        {
                            R#config_activity.id,
                            R#config_activity.week,
                            R#config_activity.month,
                            R#config_activity.day,
                            R#config_activity.hour,
                            R#config_activity.minite,
                            R#config_activity.push_c
                        } | Acc]
            end
        end,
    lists:reverse(lists:foldl(FunFoldl, [], Ids)).

get_v(Id) ->
    [R] = ets:lookup(?tab_name, Id),
    {
        R#config_activity.id,
        R#config_activity.week,
        R#config_activity.month,
        R#config_activity.day,
        R#config_activity.hour,
        R#config_activity.minite,
        R#config_activity.push_c
    }.


now_refresh_times(Id) ->
    {_Id, _W, Month, D, H, M, _PushC} = get_v(Id),
    {{NowY, NowMonth, NowD}, {NowH, NowM, _NowS}} = calendar:local_time(),
    ThisMonth = if
                    Month =:= <<"*">> -> NowMonth;
                    true -> Month
                end,
    ThisD = if
                D =:= <<"*">> -> NowD;
                true -> D
            end,
    ThisH = if
                H =:= <<"*">> -> NowH;
                true -> H
            end,
    ThisM = if
                M =:= <<"*">> -> NowM;
                true -> M
            end,
    
    erl_time:localtime_to_now({{NowY, ThisMonth, ThisD}, {ThisH, ThisM, 0}}).


diff_time(Id) ->
    {Id, W, Month, D, H, M, _PushC} = get_v(Id),
    diff_time(W, Month, D, H, M).


%% 每小时触发
diff_time(<<"*">>, <<"*">>, <<"*">>, <<"*">>, M) ->
    {_NowH, NowM, NowS} = erlang:time(),
    DiffTime = if
                   M > NowM -> M * 60 - (NowM * 60 + NowS);
                   true ->
                       (3600 - NowM * 60 - NowS) + M * 60
               end,
    {cycle, DiffTime};

%% 每天触发
diff_time(<<"*">>, <<"*">>, <<"*">>, H, M) ->
    {NowH, NowM, NowS} = erlang:time(),
    Seconds = H * 3600 + M * 60,
    NowSeconds = NowH * 3600 + NowM + 60 + NowS,
    
    DiffTime = if
                   Seconds > NowSeconds -> Seconds - NowSeconds;
                   true ->
                       (86400 - NowSeconds) + Seconds
               end,
    {cycle, DiffTime};

%% 每月触发
diff_time(<<"*">>, <<"*">>, D, H, M) ->
    {{NowYear, NowMonth, NowD}, {NowH, NowM, NowS}} = calendar:local_time(),
    Days = if
               D > NowD -> D - NowD;
               true ->
                   {NextYear, NextMonth} = next_month(NowYear, NowMonth),
                   D1 = calendar:date_to_gregorian_days({NowYear, NowMonth, NowD}),
                   D2 = calendar:date_to_gregorian_days({NextYear, NextMonth, D}),
                   D2 - D1
           end,
    DiffTime = Days * 86400 + H * 3600 + M * 60 - NowH * 3600 - NowM * 60 - NowS,
    {cycle, DiffTime};

%% 固定时间触发
diff_time(<<"*">>, Month, D, H, M) ->
    {{NowYear, NowMonth, NowD}, {NowH, NowM, NowS}} = calendar:local_time(),
    D1 = calendar:date_to_gregorian_days({NowYear, NowMonth, NowD}),
    D2 = calendar:date_to_gregorian_days({NowYear, Month, D}),
    DiffTime = (D2 - D1) * 86400 + H * 3600 + M * 60 - NowH * 3600 - NowM * 60 - NowS,
    {once, DiffTime};

%% 每周触发
diff_time(Week, <<"*">>, <<"*">>, H, M) ->
    {{NowYear, NowMonth, NowD}, {NowH, NowM, NowS}} = calendar:local_time(),
    
    NowWeek = calendar:day_of_the_week({NowYear, NowMonth, NowD}),
    Days = if
               NowWeek > Week -> (7 - NowWeek + Week);
               true -> (Week - NowWeek)
           end,
    DiffTime = Days * 86400 + H * 3600 + M * 60 - NowH * 3600 - NowM * 60 - NowS,
    {cycle, DiffTime}.


next_month(Year, 12) -> {Year + 1, 1};
next_month(Year, M) -> {Year, M + 1}.