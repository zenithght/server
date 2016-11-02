%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%%
%%% Created : 19. 七月 2016 下午2:44
%%%-------------------------------------------------------------------

-define(tab_name, player_item).
-define(tab_last_name, item_20160728).


-record(item, {
    uid,
    item_list = []
}).


-record(item_20160728, {
    id = 0,
    uid,
    item_id = 0,
    num = 0,
    state = 0,
    c_times = 0
}).


