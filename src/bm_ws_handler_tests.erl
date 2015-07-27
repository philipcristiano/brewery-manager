-module(bm_ws_handler_tests).
-include_lib("eunit/include/eunit.hrl").

-define(bm_ws_handler, ?MODULE).

nominal_test() ->
    [] = ?MODULE:delete_by_value(val, []),
    [] = ?MODULE:delete_by_value(val, [{foo, val}]),
    [] = ?MODULE:delete_by_value(val, [{foo, bar}]).
