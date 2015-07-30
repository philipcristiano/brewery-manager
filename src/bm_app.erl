-module(bm_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	{ok, Pid} = bm_sup:start_link(),
    GPPORT = application:get_env(bm, graphite_port, 2003),
    {ok, _} = ranch:start_listener(graphite_input, 1,
        ranch_tcp, [{port, GPPORT}], bm_graphite_protocol, []),
    TCPPORT = application:get_env(bm, tcp_port, 7645),

    {ok, _} = ranch:start_listener(tcp_input, 1,
        ranch_tcp, [{port, TCPPORT}], bm_tcp_protocol, []),

    {ok, _} = start_cowboy(),
    {ok, Pid}.

stop(_State) ->
	ok.

start_cowboy() ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", bm_handler_index, []},
               {"/static/[...]", cowboy_static, {dir, "priv/static/"}},
               {"/ws", bm_ws_handler, []}
        ]}
    ]),
    CBHTTP = application:get_env(bm, http_port, 8080),
    cowboy:start_http(bm_http_listener, 100, [{port, CBHTTP}],
        [{env, [{dispatch, Dispatch}]}]
    ).
