%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%%
%%% Created : 19. 七月 2016 下午2:42
%%%-------------------------------------------------------------------
-module(load_item).

-include_lib("ets_cache/include/ets_cache.hrl").
-include("pub.hrl").
-include("load_item.hrl").

-export([load_data/1, save_data/1, delete/2, del_data/1]).

-export([add_item/3, add_item_not_online/3, del_item/3, get_v/2]).

-export([item_list/1]).


load_ets_table() ->
    [
        #ets_cache{
            name = ?tab_name,
            key_pos = #item.uid
        }
    ].

load_data(Uid) ->
    Record = case erl_mysql:ed(<<"select id, uid, item_id, num, state, c_times from item where uid = ", (integer_to_binary(Uid))/binary, ";">>) of
                 [] ->
                     #item{uid = Uid};
                 ItemList ->
                     Fun =
                         fun([Id, Uid1, ItemId, Num, State, Times]) ->
                             #?tab_last_name{id = Id, uid = Uid1, item_id = ItemId, num = Num, state = State, c_times = Times}
                         end,
                     NewItemList = lists:map(Fun, ItemList),
                     #item{uid = Uid, item_list = NewItemList}
             end,
    insert(Record).


save_data(Uid) ->
    #item{item_list = ItemList} = lookup(Uid),
    Fun =
        fun(Record) ->
            NewRecord = erl_record:diff_record(Record, #?tab_last_name{}),
            case NewRecord#?tab_last_name.id of
                0 ->
                    <<"INSERT INTO item (uid, item_id, num, c_times) VALUES (",
                        (integer_to_binary(NewRecord#?tab_last_name.uid))/binary, ", ",
                        (integer_to_binary(NewRecord#?tab_last_name.item_id))/binary, ", ",
                        (integer_to_binary(NewRecord#?tab_last_name.num))/binary, ", ",
                        (integer_to_binary(NewRecord#?tab_last_name.c_times))/binary, ")">>;
                _ ->
                    <<"UPDATE item SET num = ",
                        (integer_to_binary(NewRecord#?tab_last_name.num))/binary, " WHERE id = ",
                        (integer_to_binary(NewRecord#?tab_last_name.id))/binary, ";">>
            end
        end,
    
    case lists:map(Fun, ItemList) of
        [] -> ok;
        Sql -> erl_mysql:ed(Sql)
    end.


del_data(Uin) ->
    ets_cache:delete(?tab_name, Uin).

delete(Uid, Id) ->
    #item{item_list = ItemList} = lookup(Uid),
    NewItemList = lists:keydelete(Id, #?tab_last_name.id, ItemList),
    NewRecord = #item{item_list = NewItemList},
    insert(NewRecord),
    %%重新组装
    erl_mysql:ed(<<"delete from item where id = ", (integer_to_binary(Id))/binary, ";">>).


lookup(Uid) ->
    [Item = #item{item_list = ItemList}] = ets_cache:lookup(?tab_name, Uid),
    Item#item{item_list = [erl_record:diff_record(Record, #?tab_last_name{}) || Record <- ItemList]}.


insert(Record) ->
    ets_cache:insert(?tab_name, Record).


add_item(State, PrizeId, AddItems) ->
    Uid = State#player_state.uid,
    Item = #item{item_list = Items} = lookup(Uid),
    {SendKv, NewItems} = lists:foldl(
        fun({ItemId, Num}, {Kv, ItemAcc}) ->
            trigger_add_item(ItemId, Num, ItemAcc, Kv, Uid)
        end,
        {[], Items},
        AddItems),
    insert(Item#item{item_list = NewItems}),
    log_sup:add_item_log(Uid, add, PrizeId, AddItems),
    item_handler:send_item(State, SendKv).


add_item_not_online(Uid, PrizeId, AddItems) ->
    Now = integer_to_binary(erl_time:now()),
    UidBin = integer_to_binary(Uid),
    
    Sql = lists:foldl(
        fun({ItemId, Num}, SqlAcc) ->
            NumBin = integer_to_binary(Num),
            <<SqlAcc/binary,
                "INSERT INTO item (uid, item_id, num, c_times) VALUES (", UidBin/binary, ", ", (integer_to_binary(ItemId))/binary, ", ", NumBin/binary, ", ", Now/binary, ") ON DUPLICATE KEY UPDATE num = num + ", NumBin/binary, ";">>
        end,
        <<>>,
        AddItems),
    erl_mysql:ed(Sql),
    log_sup:add_attr_log(Uid, add, PrizeId, AddItems).


del_item(State, CostId, DelItems) ->
    Uid = State#player_state.uid,
    Item = #item{item_list = Items} = lookup(Uid),
    {SendKv, NewItems} = lists:foldl(
        fun({ItemId, Num}, {Kv, ItemAcc}) ->
            trigger_del_item(ItemId, -Num, ItemAcc, Kv)
        end,
        {[], Items},
        DelItems),
    insert(Item#item{item_list = NewItems}),
    log_sup:add_item_log(Uid, del, CostId, DelItems),
    item_handler:send_item(State, SendKv).


trigger_add_item(ItemId, AddNum, Items, Kv, Uid) ->
    MaxNum = config_item:get_max_num(ItemId),
    case lists:keyfind(ItemId, #?tab_last_name.item_id, Items) of
        false ->
            NewNum = if
                         MaxNum =:= 0 -> AddNum;
                         true ->
                             if
                                 MaxNum >= AddNum -> AddNum;
                                 true -> MaxNum
                             end
                     end,
            {
                [[ItemId, NewNum] | Kv],
                [#?tab_last_name{uid = Uid, item_id = ItemId, num = NewNum} | Items]
            };
        ItemRecord ->
            Num = ItemRecord#?tab_last_name.num + AddNum,
            NewNum = if
                         MaxNum =:= 0 -> Num;
                         true ->
                             if
                                 MaxNum >= Num -> Num;
                                 true -> MaxNum
                             end
                     end,
            {
                [[ItemId, NewNum] | Kv],
                lists:keystore(ItemId, #?tab_last_name.item_id, Items, ItemRecord#?tab_last_name{num = NewNum})
            }
    end.


trigger_del_item(ItemId, DelNum, Items, Kv) ->
    Now = erl_time:now(),
    case lists:keyfind(ItemId, #?tab_last_name.item_id, Items) of
        [] ->
            {Kv, Items};
        ItemRecord ->
            Num = ItemRecord#?tab_last_name.num + DelNum,
            NewNum = if
                         Num =< 0 -> 0;
                         true ->
                             Num
                     end,
            {
                [[ItemId, NewNum] | Kv],
                lists:keystore(ItemId, #?tab_last_name.item_id, Items, ItemRecord#?tab_last_name{num = NewNum, c_times = Now})
            }
    end.


item_list(Uid) when is_integer(Uid) ->
    #item{item_list = Items} = lookup(Uid),
    [[ItemId, Num] || #?tab_last_name{item_id = ItemId, num = Num} <- Items].


get_v(Uid, ItemId) when is_integer(ItemId) ->
    #item{item_list = Items} = lookup(Uid),
    case lists:keyfind(ItemId, #?tab_last_name.item_id, Items) of
        false -> 0;
        R -> R#?tab_last_name.num
    end;

get_v(Uid, ItemIds) when is_list(ItemIds) ->
    #item{item_list = Items} = lookup(Uid),
    Fun =
        fun(ItemId) ->
            case lists:keyfind(ItemId, #?tab_last_name.item_id, Items) of
                false -> 0;
                R -> R#?tab_last_name.num
            end
        end,
    lists:map(Fun, ItemIds).