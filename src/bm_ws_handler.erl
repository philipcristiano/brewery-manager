-module(bm_ws_handler).
-behaviour(cowboy_websocket_handler).

-compile([{parse_transform, lager_transform}]).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).
-export([delete_by_value/2]).

-record(state, {device_pids}).

init({tcp, http}, _Req, _Opts) ->
    % join_all(),
	{upgrade, protocol, cowboy_websocket}.

join([]) ->
    ok;
join([{bm_devices, Name}|T]) ->
    pg2:join({bm_devices, Name}, self()),
    join(T);
join([_H|T]) ->
    join(T).

join_once(Group) ->
    io:format("Join: ~p~n", [Group]),

    Members = pg2:get_members(Group),
    case lists:member(self(), Members) of
        true -> ok;
        false -> pg2:join(Group, self())
    end.

sync_membership([]) ->
    ok;
sync_membership([Group | Groups]) ->
    join([]),
    Name = {bm_devices, proplists:get_value(<<"id">>, Group)},
    Selected = proplists:get_value(<<"enabled">>, Group, false),
    ok = case Selected of
            true -> join_once(Name);
            _ -> io:format("Leave: ~p~n", [Name]),
                 pg2:leave(Name, self())
    end,
    sync_membership(Groups).

%groups_to_proplists(Groups) ->
%    [[{id, ID}] || {_, ID} <- Groups].

websocket_init(_TransportName, Req, _Opts) ->
	% erlang:start_timer(1000, self(), <<"Hello!">>),
    self() ! send_groups,
	{ok, Req, #state{device_pids=[]}}.

%% Handle messages from client
websocket_handle({text, String}, Req, State) ->
    io:format("Message: ~p~n", [String]),
    Data = jsx:decode(String),
    io:format("Data: ~p~n", [Data]),
    handle_client_msg(proplists:get_value(<<"type">>, Data, unknown), Data, State),
	{ok, Req, State}.

handle_client_msg(unknown, Data, _State) ->
    lager:debug("Unknown client command: ~p~n", Data);
handle_client_msg(<<"set">>, Data, _State=#state{device_pids=Pids}) ->
    Device = proplists:get_value(<<"device">>, Data),
    Pid = proplists:get_value(Device, Pids),

    Group = proplists:get_value(<<"group">>, Data),
    Param = proplists:get_value(<<"parameter">>, Data),
    Value = proplists:get_value(<<"value">>, Data),

    lager:debug("Setting settable: ~p~n", [Data]),
    bm_tcp_protocol:set_settable(Pid, Device, Group, Param, Value);
handle_client_msg(<<"membership">>, Data, _State) ->
    lager:debug("Syncing membership: ~p~n", Data),
    sync_membership(proplists:get_value(<<"data">>, Data)),
    ok;
handle_client_msg(Type, Data, _State) ->
    lager:debug("Unknown client command: ~p~p~n", [Type, Data]).

%% Handle messages from VM
websocket_info(send_groups, Req, State) ->
    Groups = pg2:which_groups(),
    lager:debug("Found groups: ~p~n", [Groups]),
    Msg = [{type, groups}, {data, pg_groups_to_json(Groups)}],
    lager:debug("Encoding message: ~p~n", [Msg]),
    {ok, _} = timer:send_after(5000, send_groups),
    {reply, {text, jsx:encode(Msg)}, Req, State};
websocket_info({pipe, Pipe, {_Key, Msg}}, Req, State) ->
    Send = [{type, event}, {pipe, [Pipe]}, {data, Msg}],
    lager:debug("Sending: ~p~n", [Send]),
    {reply, {text, jsx:encode(Send)}, Req, State};
websocket_info({pipe, Pipe, Data}, Req, State) ->
    Send = [{type, event}, {pipe, [Pipe]}, {data, Data}],
    lager:debug("Sending: ~p~n", [Data]),
    {reply, {text, jsx:encode(Send)}, Req, State};
websocket_info({settable, Name, {From, Msg}}, Req, State=#state{device_pids=Pids}) ->
    Device = proplists:get_value(<<"device">>, Msg),
    Pids2 = case proplists:is_defined(Device, Pids)  of
                false -> erlang:monitor(process, From),
                         [{Device, From} | Pids];
                _ -> Pids
    end,
    Send = [{type, settable}, {pipe, [Name]}, {data, Msg}],
    lager:debug("Sending: ~p~n", [Msg]),
    {reply, {text, jsx:encode(Send)}, Req, State#state{device_pids=Pids2}};
websocket_info({'DOWN', _MonitorRef, process, Pid, _Info}, Req, State=#state{device_pids=Pids}) ->
    lager:debug("Monitor down pids ~p~n", [Pids]),
    NewPids = delete_by_value(Pid, Pids),
    lager:debug("Monitor down new pids ~p~n", [NewPids]),
	{ok, Req, State#state{device_pids=NewPids}};
websocket_info(Info, Req, State) ->
    lager:info("Websocket unhandled message: ~p~n", [Info]),
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.

pg_groups_to_json([]) ->
    [];
pg_groups_to_json([{bm_devices, Name}|Groups]) ->
    [Name | pg_groups_to_json(Groups)].

delete_by_value(_Val, []) ->
    [];
delete_by_value(Val, [{_, Val}|Rest]) ->
    delete_by_value(Val, Rest);
delete_by_value(Val, [{Head, Val2}|Rest]) ->
    [{Head, Val2}| delete_by_value(Val, Rest)].
