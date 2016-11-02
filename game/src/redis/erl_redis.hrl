%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%%
%%% Created : 18. 九月 2016 上午11:13
%%%-------------------------------------------------------------------


-define(TAB_ONLINE(PlayerId),       <<"online:", PlayerId/binary>>).
-define(node,       <<"node">>).
-define(pid,        <<"pid">>).

-define(TAB_ATTR(PlayerId),         <<"attr:", PlayerId/binary>>).
-define(TAB_RANK(RankName),         <<"rank:", RankName/binary>>).
-define(TAB_OFFLINE(PlayerId),      <<"offline:", PlayerId/binary>>).
-define(type,       <<"type">>).
-define(msg,        <<"msg">>).


-define(TAB_BLACKLIST(PlayerId),    <<"blacklist:", PlayerId/binary>>).


-define(TAB_NICK(Nick),              <<"nick:", Nick/binary>>).


-define(TAB_CHAT_MSG(Uid),          <<"chat_msg:", Uid/binary>>).

-define(EXPIRE_TIME,                259200).%设置3天后销毁该数据，中途覆盖该时间
-define(EXPIRE_TIME_2,              604800).   %86400*7 中途覆盖该时间