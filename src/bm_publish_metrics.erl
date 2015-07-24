-module(bm_publish_metrics).

-compile([{parse_transform, lager_transform}]).

-export([publish_temperature/2,
         publish_temperature/4]).


publish_temperature(ID, Temp) ->
    T = etsdb_numbers:to_float(Temp),
    Topic = {bm_devices, ID},
    pg2:create(Topic),
    Pids = pg2:get_members(Topic),
    send(Pids, temperature, {ID, T}).

publish_temperature(Device, Sensor, TS, Temp) ->
    lager:debug("Publishing temperature ~p~n", [{Device, Sensor, TS, Temp}]),
    T = etsdb_numbers:to_float(Temp),
    Topic = {bm_devices, Device},
    pg2:create(Topic),
    Pids = pg2:get_members(Topic),

    Data = [{<<"device">>, Device},
            {<<"sensor">>, Sensor},
            {<<"value">>, T},
            {<<"timestamp">>, TS}],

    send(Pids, temperature, Data).

send([], _Name, _Msg) ->
    ok;
send([Pid|Pids], Name, Msg) ->
    Pid ! {pipe, Name, Msg},
    send(Pids, Name, Msg).
