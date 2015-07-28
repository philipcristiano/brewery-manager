-module(bm_ws_handler_tests).
-include_lib("eunit/include/eunit.hrl").

-define(MUT, bm_ws_handler).

nominal_test() ->
    [] = ?MUT:delete_by_value(val, []),
    [] = ?MUT:delete_by_value(val, [{foo, val}]),
    [{foo, bar}] = ?MUT:delete_by_value(val, [{foo, bar}]).
