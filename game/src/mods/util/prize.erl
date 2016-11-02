%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc 资源产出
%%%
%%% Created : 22. 七月 2016 下午5:02
%%%-------------------------------------------------------------------
-module(prize).

-include("pub.hrl").

-export([prize_other_pid/2, prize/2, prize/3]).


prize_other_pid(Uid, OrderId) ->
    case redis_online:is_online(Uid) of
        {ok, Pid} ->
            ?send_mod_msg(Pid, ?item_handler, {?add_item, OrderId});
        {ok, Node, Pid} ->
            node_srv:send_to_node(Uid, Pid, Node, ?mod_msg(?item_handler, {?add_item, OrderId}));
        false ->
            GoodsId = load_orders:select_goods_id(OrderId),
            PrizeId = config_shop:get_prize_id(GoodsId),
            item_handler:add_item_not_online(Uid, PrizeId),
            load_orders:update_orders(OrderId)
    end.

prize(_State, 0) -> ok;
prize(State, PrizeId) when is_integer(PrizeId) ->
    PrizeList = config_prize:get_prize_list(PrizeId),
    item_handler:add_item(State, PrizeId, PrizeList).

prize(State, PrizeId, PrizeList) ->
    item_handler:add_item(State, PrizeId, PrizeList).

