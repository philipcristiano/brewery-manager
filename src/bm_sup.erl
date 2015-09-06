-module(bm_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [{bm_sup_connections,
              {bm_sup_connections, start_link, []},
               permanent, 5000, supervisor, [bm_sup_connections]}],
	{ok, {{one_for_one, 1, 5}, Procs}}.
