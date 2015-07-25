-module(bm_tcp_protocol).
-behaviour(ranch_protocol).

-compile([{parse_transform, lager_transform}]).

-export([start_link/4]).
-export([init/4]).

-record(state, {buffer, device=undef}).

start_link(Ref, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
	{ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
    lager:debug("New TCP Connection~n"),
	ok = ranch:accept_ack(Ref),
	loop(Socket, Transport, #state{buffer = <<"">>}).

loop(Socket, Transport, State) ->
    lager:debug("Loop state ~p~n", [State]),
	case Transport:recv(Socket, 0, 3600000) of
		{ok, Data} ->
            {ok, NewState} = handle_data(Data, State),
			loop(Socket, Transport, NewState);
		A ->
            lager:debug("Graphite huh? ~p~n", [A]),
			ok = Transport:close(Socket)
	end.

handle_data(NewData, State=#state{buffer=Buffer}) ->
    lager:debug("Handle data: ~p~n", [NewData]),
    Data = <<Buffer/binary, NewData/binary>>,
    {ok, NewState} = parse_data(Data, State),
    lager:debug("Done parsing"),
    {ok, NewState}.

parse_data(Data, State) ->
    lager:debug("Parse binary: ~p~n", [Data]),
    case binary:split(Data, <<"\n">>) of
        [Line, Rest] ->
            {ok, NewState} = parse_line(Line, State),
            parse_data(Rest, NewState);
        [_] ->
            lager:debug("No match"),
            {ok, State#state{buffer=Data}}
    end.

parse_line(Line, State) ->
    [Type| Rest] = binary:split(Line, [<<":">>], [global]),
    {ok, NewState} = handle_message(Type, Rest, State),

    %% [Metric, Value, TS] = binary:split(Messages, [<<" ">>], [global]),
    %% NumTS = case erlang:binary_to_integer(TS) of
    %%             0 -> get_timestamp();
    %%             A -> A
    %% end,
    %% bm_tsdb_server:write(Metric, NumTS, etsdb_numbers:to_float(Value)),
    {ok, NewState}.

handle_message(<<"device">>, [DeviceID], State) ->
    {ok, State#state{device=DeviceID}};
handle_message(<<"temperature (c)">>, [Sensor, Value], State=#state{device=Device}) ->
    lager:debug("Temp State: ~p~n", [State]),
    Now = get_timestamp(),
    bm_publish_metrics:publish_temperature(Device, Sensor, Now, Value),
    {ok, State};
handle_message(<<"settable">>, [Group, Parameter], State=#state{device=Device}) ->
    lager:debug("settable State: ~p~n", [State]),
    bm_publish_metrics:settable_parameter(Device, Group, Parameter),
    {ok, State};
handle_message(Unknown, Data, State) ->
    lager:info("TCP protocol receiving unknown message ~p, ~p ~n", [Unknown, Data]),
    {ok, State}.

get_timestamp() ->
  {Mega, Sec, _Micro} = os:timestamp(),
  (Mega*1000000 + Sec).
