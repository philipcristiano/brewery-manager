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
	case Transport:recv(Socket, 0, 3600000) of
		{ok, Data} ->
            {ok, NewState} = handle_data(State, Data),
			loop(Socket, Transport, NewState);
		A ->
            lager:debug("Graphite huh? ~p~n", [A]),
			ok = Transport:close(Socket)
	end.

handle_data(State=#state{buffer=Buffer}, NewData) ->
    lager:debug("Handle data: ~p~n", [NewData]),
    Data = <<Buffer/binary, NewData/binary>>,
    Unprocessed = parse_data(Data),
    {ok, State#state{buffer=Unprocessed}}.

parse_data(Data) ->
    lager:debug("Parse binary: ~p~n", [Data]),
    case binary:split(Data, <<"\n">>) of
    [_] ->
        Data;
    [Line, Rest] ->
        write_message(Line),
        parse_data(Rest)
    end.

write_message(Message) ->
    lager:debug("Parsing message: ~p~n", [Message]),
    %% [Metric, Value, TS] = binary:split(Messages, [<<" ">>], [global]),
    %% NumTS = case erlang:binary_to_integer(TS) of
    %%             0 -> get_timestamp();
    %%             A -> A
    %% end,
    %% bm_tsdb_server:write(Metric, NumTS, etsdb_numbers:to_float(Value)),
    ok.

%% get_timestamp() ->
%%   {Mega, Sec, _Micro} = os:timestamp(),
%%   (Mega*1000000 + Sec).
