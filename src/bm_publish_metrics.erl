-module(bm_publish_metrics).

-export([publish_temperature/2]).


publish_temperature(ID, Temp) ->
    T = etsdb_numbers:to_float(Temp),
    Topic = {bm_temperature, ID},
    pg2:create(Topic),
    Pids = pg2:get_members(Topic),
    send(Pids, temperature, {ID, T}).

send([], _Name, _Msg) ->
    ok;
send([Pid|Pids], Name, Msg) ->
    Pid ! {pipe, Name, Msg},
    send(Pids, Name, Msg).
