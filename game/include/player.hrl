%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc 玩家信息
%%%
%%% @end
%%% Created : 24. 六月 2016 下午2:55
%%%-------------------------------------------------------------------


-define(LOGIN_DEFAULT,      0). %0默认设置
-define(LOGIN_DONE,         1). %表示登录校验完成
-define(LOGIN_CREATE_ROLE,  2). %判断是否设置昵称，没有设置需要设置，然后才能开放后续功能(未实现)
-define(LOGIN_INIT_DONE,    3). %表示数据初始化完成，可以收发数据，开放所有功能
-define(PLAYER_TERMINATE,   4). %玩家下线


-record(player_state, {
    tick = 0,
    socket,                             %
    uid = 0,
    login_state = ?LOGIN_DEFAULT        %登录状态
}).


-define(ALL_PLAYER_MODS, []).


%% 玩家进程字典


%% frame_event


