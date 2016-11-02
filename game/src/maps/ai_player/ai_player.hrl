%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%%
%%% Created : 19. 八月 2016 下午2:30
%%%-------------------------------------------------------------------

%% 一条ai蛇的全局状态
-define(AI_PLAYER_INIT,         1). %% 1.初始化
-define(AI_PLAYER_ENTER_MAP,    2). %% 2.进入地图、玩耍
-define(AI_PLAYER_DIE,          3). %% 3.死亡
-define(AI_PLAYER_LEAVE,        4). %% 4.离开

%% 在地图中 2
-define(AI_PLAYER_PLAY,         1). %% 1.正常状态
-define(AI_PLAYER_SUSPEND,      2). %% 2.挂起状态。地图进程没有玩家，挂起ai进程
-define(AI_PLAYER_WAITING,      3). %% 3.等待数据返回状态。 向地图进程要数据


-define(RANDOM_FPS,             6).


-record(ai_player, {
    state = ?AI_PLAYER_INIT,
    state_1 = ?AI_PLAYER_PLAY,
    queue_pid,
    map_pid,
    player_id,
    map_r,
    ets_player,
    ets_food,
    move,
    skill = 0,
    angle = 0,
    is_speed = 0,
    cost_fps = 0    %某项执行需要多少帧
}).