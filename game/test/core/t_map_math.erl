%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%%
%%% Created : 03. 九月 2016 上午10:59
%%%-------------------------------------------------------------------
-module(t_map_math).

-export([t/0]).
%%far
%%erl_time:908411
%%nif_time:855752
%%erl_time:888643
%%nif_time:805556

%%out_c
%%erl_time:543288
%%nif_time:1451884
%%erl_time:551762
%%nif_time:1381869


%%in_c
%%erl_time:563810
%%nif_time:1496264
%%erl_time:553971
%%nif_time:1491401


%%p_in_c
%%erl_time:1105211
%%nif_time:1671068
%%erl_time:1103008
%%nif_time:1737093


%%c_in_c
%%erl_time:1188142
%%nif_time:1573858
%%erl_time:1190728
%%nif_time:1578339


%%move
%%erl_time:1718010
%%nif_time:2714678
%%erl_time:1650099
%%nif_time:2618943

%%move_body
%%erl_time:1229613
%%nif_time:1324456
%%erl_time:1211824
%%nif_time:1397825

%%get_in_chunks
%%erl_time:1931771
%%nif_time:1583958
%%erl_time:2394387
%%nif_time:1620954

%%get_xyz
%%erl_time:809543
%%nif_time:854158
%%erl_time:808339
%%nif_time:874489

%%map_chunks 100W
%%erl_time:3017810
%%nif_time:2085991
%%erl_time:2847776
%%nif_time:2145834


%%xy_to_angle
%%erl_time:1664529
%%nif_time:1346280
%%erl_time:1587681
%%nif_time:1286197


%%view_chunks
%%erl_time:257209
%%nif_time:312951
%%erl_time:272900
%%nif_time:317321

t() ->
    T1 = os:timestamp(),
    t_erl(10000000),
    T2 = os:timestamp(),
    t_nif(10000000),
    io:format("erl_time:~p~nnif_time:~p~n", [timer:now_diff(T2, T1), timer:now_diff(os:timestamp(), T2)]).

t_erl(0) ->
    ok;

t_erl(Num) ->
    A = float(Num),
%%    map_math:far(A, A, A + A, A + A),
%%    map_math:out_c(A, A, Num),
%%    map_math:in_c(A, A, Num),
%%    map_math:p_in_c(A, A, A + A, A + A, Num),
%%    map_math:c_in_c(A, A, Num, Num, A, Num),
%%    map_math:move(A, A, Num rem 360, Num),
%%    map_math:move_body(A, A, A + A, A + A, A, A, A, A),
%%    map_math:get_in_chunks(A, A, Num, Num),
%%    map_math:get_xyz(A, A, Num),
%%    map_math:map_chunks(5000, 1000),
%%    map_math:xy_to_angle(A, A, A + A, A + A),
    map_math:view_chunks(A, A, 1000, 2000),
%%    map_math:get_cur_a(0, 90, 0, 10),
    t_erl(Num - 1).

t_nif(0) -> ok;
t_nif(Num) ->
    A = float(Num),
%%    map_math_nif:move_body(A, A, A + A, A + A, A, A, A, A),
%%    map_math_nif:get_in_chunks(A, A, Num, Num),
%%    map_math_nif:get_xyz(A, A, Num),
%%    map_math_nif:map_chunks(5000, 1000),
    map_math_nif:view_chunks(A, A, 1000, 2000),
    t_nif(Num - 1).
    