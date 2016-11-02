-module(snakes_app).

-behaviour(application).

-export([start/2, stop/1, stop/0, start/0]).

-include("pub.hrl").


%% rabbitmq框架使用的tcp选项
%% -define(TCP_OPTIONS, [binary, {packet, 0}, {active, false},
%%                      {reuseaddr, true}, {nodelay, false}, {delay_send, true},
%%                      {send_timeout, 5000}, {keepalive, false}, {exit_on_close, true}]).
-define(TCP_OPTIONS, [
    binary,
    {packet, 0},            %%不设置包头长度
    {active, false},        %% 无法接收数据，直到inet:setopts(Socket, [{active, once}]), 接收一次数据
    {delay_send, true},     %%delay_send是不主动强制send, 而是等socket可写的时候马上就写 延迟发送：{delay_send, true}，聚合若干小消息为一个大消息，性能提升显著
    {nodelay, true},        %%If Boolean == true, the TCP_NODELAY option is turned on for the socket, which means that even small amounts of data will be sent immediately.
    {reuseaddr, true},
    {send_timeout, 5000},    %% 发送超时时间5s
    {high_watermark, 38528},   %% 默认8192 8kb
    {low_watermark, 19264}      %% 默认 4096 4kb

]).

start(_StartType, _StartArgs) ->
    case os:type() of
        {unix, linux} ->
            lager:start();
        _ ->
            ok
    end,
    application:start(crypto),
    application:start(inets),
    eredis_pool:start(),
    emysql:start(),
    
    ssl:start(),
    
    application:start(config),
    {ok, Pid} = snakes_sup:start_link(),
    init(),
    ?INFO("game started...~n"),
    {ok, Pid}.

stop(_State) ->
    ok.


start() ->
    application:start(snakes).

stop() ->
    application:stop(listen),
    application:stop(snakes),
    application:stop(ets_cache),
    application:stop(config),
    init:stop().


init() ->
    application:start(ets_cache),
    
    application:start(listen),
    {ok, Port} = application:get_env(snakes, port),
    listen:start(Port, ?TCP_OPTIONS, player_server, ws),
    ?INFO("listen tcp port:~p~n", [Port]),
    load_attr:init(1),
    ok.




