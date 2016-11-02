%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc 资源消耗
%%%
%%% Created : 22. 七月 2016 下午5:02
%%%-------------------------------------------------------------------
-module(cost).

-include("pub.hrl").

-export([cost/2, cost/3]).

cost(_State, 0) -> ok;
cost(State, CostId) when is_integer(CostId) ->
    CostList = config_cost:get_cost_list(CostId),
    cost(State, CostId, CostList).

cost(State, CostId, CostList) ->
    item_handler:cost_item(State, CostId, CostList).


