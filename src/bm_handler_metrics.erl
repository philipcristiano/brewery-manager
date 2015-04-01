-module(bm_handler_metrics).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    {ok, Keys} = bm_tsdb_server:metrics(),
	{ok, Req2} = cowboy_req:reply(200, [
		{<<"content-type">>, <<"application/json">>}
	], jsx:encode([{data, Keys}]), Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
