%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%%
%%% Created : 03. 八月 2016 上午10:51
%%%-------------------------------------------------------------------

-behavior(map_plugin).

-export([map_init/0, enter_map/1, leave_map/1, map_terminate/0, map_terminate/1]).

-export([handler_info/2]).