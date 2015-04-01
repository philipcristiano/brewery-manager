-module(bm_handler_metric_data).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    {Metric, _} = cowboy_req:binding(metric, Req),
    {TS1, _} = cowboy_req:qs_val(<<"from">>, Req, 0),
    {TS2, _} = cowboy_req:qs_val(<<"until">>, Req, 86400),
    {Bucket, _} = cowboy_req:qs_val(<<"bucket_size">>, Req, <<"60">>),
    Aggregation = 60,
    {ok,Data} = bm_tsdb_server:aggregate(Metric, TS1, TS2,
                      [{bucket_size, erlang:binary_to_integer(Bucket)},
                       {aggregation, Aggregation}]),
	{ok, Req2} = cowboy_req:reply(200, [
		{<<"content-type">>, <<"application/json">>}
	], jsx:encode([{data, Data}]), Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
