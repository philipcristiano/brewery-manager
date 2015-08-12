-module(bm_publish_metrics).

-compile([{parse_transform, lager_transform}]).

-export([publish_temperature/4,
         settable_parameter/5]).


-spec publish_temperature(binary(), binary(), integer(), number()) -> ok.

publish_temperature(Device, Sensor, TS, Temp) ->
    _ = lager:debug("Publishing temperature ~p~n", [{Device, Sensor, TS, Temp}]),
    Topic = {bm_devices, Device},
    pg2:create(Topic),
    Pids = pg2:get_members(Topic),

    Data = [{<<"device">>, Device},
            {<<"sensor">>, Sensor},
            {<<"value">>, Temp},
            {<<"timestamp">>, TS}],

    send(pipe, Pids, temperature, Data).

-spec settable_parameter(pid(), binary(), binary(), binary(), binary()) -> ok.

settable_parameter(From, Device, Group, Parameter, Value) ->
    _ = lager:debug("Publishing settable ~p~n", [{Device, Group, Parameter, Value}]),
    Topic = {bm_devices, Device},
    pg2:create(Topic),
    Pids = pg2:get_members(Topic),

    Data = [{<<"device">>, Device},
            {<<"group">>, Group},
            {<<"value">>, Value},
            {<<"parameter">>, Parameter}],

    send(settable, Pids, temperature, {From, Data}).


send(_Type, [], _Name, _Msg) ->
    ok;
send(Type, [Pid|Pids], Name, Msg) ->
    Pid ! {Type, Name, Msg},
    send(Type, Pids, Name, Msg).
