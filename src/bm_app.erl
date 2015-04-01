-module(bm_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	{ok, Pid} = bm_sup:start_link(),
    GPPORT = application:get_env(bm, graphite_port, 2003),
    {ok, _RanchPid} = ranch:start_listener(graphite_input, 1,
        ranch_tcp, [{port, GPPORT}], bm_graphite_protocol, []),
    start_cowboy(),
    {ok, Pid}.

stop(_State) ->
	ok.

start_cowboy() ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", bm_handler_index, []},
               {"/static/[...]", cowboy_static, {priv_dir, bm, "static/"}},
               {"/metrics", bm_handler_metrics, []},
               {"/metrics/:metric/", bm_handler_metric_data, []}
        ]}
    ]),
    CBHTTP = application:get_env(bm, http_port, 8080),
    cowboy:start_http(bm_http_listener, 100, [{port, CBHTTP}],
        [{env, [{dispatch, Dispatch}]}]
    ).
