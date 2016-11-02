%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%%
%%% Created : 18. 七月 2016 上午9:17
%%%-------------------------------------------------------------------
-module(cost_can).

-include("pub.hrl").

-export([
    assert_attr/2,
    assert_item/2
]).

assert_attr(_Uid, []) -> ok;
assert_attr(Uid, List) ->
    Indexs = [Index || {_Type, Index, _Num} <- List],
    CostNums = [Num || {_Type, _Index, Num} <- List],
    HaveNums = load_attr:get_v(Uid, Indexs),
    check_num(CostNums, HaveNums).


check_num([], _) -> ok;
check_num([CostNum | CostNums], [HaveNum | HaveNums]) ->
    if
        CostNum =< HaveNum ->
            check_num(CostNums, HaveNums);
        true ->
            ?return_err(?ERR_ATTR_NOT_ENOUGH_NUM)
    end.


assert_item(_Uid, []) -> ok;
assert_item(Uid, List) ->
    Indexs = [Index || {_Type, Index, _Num} <- List],
    CostNums = [Num || {_Type, _Index, Num} <- List],
    HaveNums = load_item:get_v(Uid, Indexs),
    check_num(CostNums, HaveNums).