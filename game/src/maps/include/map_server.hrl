%%%-------------------------------------------------------------------
%%% @author yujian
%%% @doc
%%%
%%% Created : 23. 六月 2016 下午5:29
%%%-------------------------------------------------------------------
-include("map_def.hrl").

-define(FPS,                    20).
-define(THIS_FPS,               this_fps).     %当前是第几帧


-define(tick_time,              1200000).       %地图超时时间,此时地图上没有玩家，则关闭该地图
-define(tick_rank,              5000).          %下发排行榜信息
-define(tick_refresh_ai_food,   180000).        %刷新移动食物时间

-define(INIT_BODY_LEN,          10).            %不带头部， 初始化有10节

-define(GENERATE_FOOT_UNIT,     5).             %以每五节身体为单位，掉落食物
-define(GENERATE_FOOT_MAX_NUM,  3).             %以每五节身体为单位, 每单位最大食物数量
-define(GENERATE_FOOT_MIN_SCORE,100).           %掉落积分食物，每一个食物的最小值

-define(SKILL_1_TIME,           2000).          %进入游戏时的无敌时间

-define(GENERATE_FOOT_FPS,      5).             %加速掉落食物间隔fps

-define(AI_PLAYER_NUM,          19).            %每個地圖中最多有19个ai

-record(map_queue, {
    map_pid,            %地图pid
    self_pid = self(),  %队列进程pid
    map_type = ?MAP_NORMAL,
    stimes,             %地图开始时间
    ai_num = 0,         %ai人数
    player_num = 0,     %玩家人数
    player_list = [],   %玩家列表 [pid1,pid2]
    watch_num = 0,      %观战人数
    watch_list = [],    %观战列表
    map_r,              %地图半径
    map_chunk_weight,   %地图中一个块的大小
    deadline,           %截至时间
    rank_pid           %排行榜进程
}).


-record(player, {
    id,                                 % 玩家在该地图的唯一id
    config_speed,
    config_speed_up,
    config_angle,
    config_r,
    config_gap,
    config_len,
    config_cost_score,
    pid,                                % 玩家进程pid
    uid = 0,                            % 玩家uid
    player_type = ?PLAYER_TYPE_ROLE,    % 0机器人， 1玩家
    name,
    lv = 1,                             % 等级影响半径和长度
    model = 0,                          % 蛇模型
    socket = 0,                         % 玩家socket，直接在map进程发送数据给客户端（是否可以提高性能）
    state = ?PLAYER_GOD,                % 0 死亡 1进入地图 2.初始化完成，玩家游戏 3玩家离开地图 4无敌效果 5处于观战模式
    is_speed = ?PLAYER_NORMAL_SPEED,    % 是否加速
    speed_up_fps = 0,                      % 加速状态度过的帧数
    cur_a = 0,                          % 头部转动角度
    target_a,                           % 需要转到的方向
    add_a,                              % 角度加速度,如果以后做惯性拐外的话,用到,现在没用
    head,                               % #obstacle{}
    bodys,                              % [#obstacle{}...]
    body_len = ?INIT_BODY_LEN,          % 不带头部， 初始化有10节
    score = 0,                          % 得分
    history_max_score = 0,              % 历史最大得分
    history_max_body_len = 0,           % 历史最大长度
    kill_num = 0,                       % 击杀数
    killed_num = 0,                     % 被击杀数
    e_times = 0,                        % 进入地图时间戳
    timer_ref,                          % 倒计时时间戳
    pos_list = [],                      % 客户端发过来的位置数据列表
    delayFrame = 0,                     % 客户端和服务器端的延迟帧数
    have_pos = 0,                       % 本次计算,服务器端收到客户端的数据时=1, 否则=0
    kill_player = 0,                    % 被玩家杀掉
    killed_player = []                  % 杀掉的玩家id列表，并且没有重新开始或者退出的玩家
}).


-record(obstacle, {
    x,
    y,
    r,
    in_chunks = []   %位于哪些块中
}).


%% 1.可以重叠在一起。
%% 2.随机刷新 某个块多少个 每个多少等级
%% 3.玩家死亡时根据玩家等级生产food
-record(food, {
    id,     %food在地图中的id
    x,      %x坐标
    y,      %y坐标
    lv,
    r,      %food半径
    score,  %food拥有积分
    model   %模型
}).


%%1.移动食物
-record(ai_food, {
    id,             %食物id
    x,              %x轴
    y,              %y轴
    lv,             %等级
    r,              %半径
    score,          %积分
    model,          %模型
    cur_a,          %移动角度,根据视野范围中的蛇头计算下一个移动角度
    target_a,       %%初始化方向，当没有被人追赶时，朝向该方向移动，直到地图外
    this_fps = 0,
    view_r,         %视野范围
    in_chunk        %%只会在一个块中{Al, ChunkX, ChunkY}
}).
