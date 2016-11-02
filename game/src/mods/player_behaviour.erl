%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc 玩家模块，通用行为规范
%%% 每一个模块都会有的功能：加载数据(当没有数据时，创建数据,创建角色时触发）， 上线初始化信息，下线处理，地图中他人看到的自己的信息
%%%
%%% @end
%%% Created : 24. 六月 2016 下午2:55
%%%-------------------------------------------------------------------

-module(player_behaviour).

-include("pub.hrl").

-export([
    load_data/1,
    online/1,
    online_send_data/1,
%%    save_data/1,
    terminate/1,
    handler_frame/2
]).

%% frame_event, 方便查看
-export([save_data/1, event_lvup/1, event_zero_refresh/1]).


%% @doc 加载数据(当没有数据时，创建数据,创建角色时触发）
-callback load_data(State :: #player_state{}) -> State :: #player_state{}.


%% @doc 上线初始化信息
-callback online(State :: #player_state{}) -> State :: #player_state{}.


%% @doc 发送信息给客户端,考虑玩家需要迁移服务器的情况。不用下发数据给客户端
-callback online_send_data(State :: #player_state{}) -> State :: #player_state{}.


%% @doc 数据持久化
-callback save_data(State :: #player_state{}) -> State :: #player_state{}.


%% @doc 下线处理
-callback terminate(State :: #player_state{}) -> State :: #player_state{}.

%%
%%%% @doc 地图中他人看到的自己的信息,各个模块可能都会有信息
%%%% 举例：别人能看到自己的：角色、装备、工会、称号信息。
%%-callback view_data(Term :: binary()) -> Term :: binary().


%% @doc 事件触发。目前有：零时刷新数据
-callback handler_frame(State :: #player_state{}, EventName :: atom()) -> term().


%% @doc 其他进程发送到该模块的信息
-callback handler_msg(State :: #player_state{}, FromPid :: pid(), FromModule :: atom(), Msg :: term()) -> term().


load_data(State) ->
    lists:foldl(
        fun(Mod, State1) ->
            Mod:load_data(State1)
        end,
        State,
        ?ALL_PLAYER_MODS).


online(State) ->
    lists:foldl(
        fun(Mod, State1) ->
            Mod:online(State1)
        end,
        State,
        ?ALL_PLAYER_MODS).


online_send_data(State) ->
    lists:foldl(
        fun(Mod, State1) ->
            Mod:online_send_data(State1)
        end,
        State,
        ?ALL_PLAYER_MODS).


terminate(State) ->
    lists:foldl(
        fun(Mod, State1) ->
            Mod:terminate(State1)
        end,
        State,
        ?ALL_PLAYER_MODS).


handler_frame(State, EventName) ->
    lists:foldl(
        fun(Mod, State1) ->
            Mod:handler_frame(State1, EventName)
        end,
        State,
        ?ALL_PLAYER_MODS).


event_lvup(State) ->
    handler_frame(State, ?event_lvup).


event_zero_refresh(State) ->
    handler_frame(State, ?event_zero_refresh).


%% @doc 每隔多少分钟持久化数据
save_data(State) -> lists:foldl(
    fun(Mod, State1) ->
        Mod:save_data(State1)
    end,
    State,
    ?ALL_PLAYER_MODS).