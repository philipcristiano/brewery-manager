-module(bm_ws_handler).
-behaviour(cowboy_websocket_handler).

-compile([{parse_transform, lager_transform}]).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

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
    case Selected of
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
	{ok, Req, undefined_state}.

%% Handle messages from client
websocket_handle({text, String}, Req, State) ->
    io:format("Message: ~p~n", [String]),
    Data = jsx:decode(String),
    io:format("Data: ~p~n", [Data]),
    handle_client_msg(proplists:get_value(<<"type">>, Data, unknown), Data),
	{ok, Req, State}.

handle_client_msg(unknown, Data) ->
    lager:debug("Unknown client command: ~p~n", Data);
handle_client_msg(<<"membership">>, Data) ->
    lager:debug("Syncing membership: ~p~n", Data),
    sync_membership(proplists:get_value(<<"data">>, Data)),
    ok.

%% Handle messages from VM
websocket_info(send_groups, Req, State) ->
    Groups = pg2:which_groups(),
    lager:debug("Found groups: ~p~n", [Groups]),
    Msg = [{type, groups}, {data, pg_groups_to_json(Groups)}],
    lager:debug("Encoding message: ~p~n", [Msg]),
    timer:send_after(5000, send_groups),
    {reply, {text, jsx:encode(Msg)}, Req, State};
websocket_info({pipe, Pipe, {_Key, Msg}}, Req, State) ->
    Send = [{type, event}, {pipe, [Pipe]}, {data, Msg}],
    lager:debug("Sending: ~p~n", [Send]),
    {reply, {text, jsx:encode(Send)}, Req, State};
websocket_info({pipe, Pipe, Data}, Req, State) ->
    Send = [{type, event}, {pipe, [Pipe]}, {data, Data}],
    lager:debug("Sending: ~p~n", [Data]),
    {reply, {text, jsx:encode(Send)}, Req, State};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.

pg_groups_to_json([]) ->
    [];
pg_groups_to_json([{bm_devices, Name}|Groups]) ->
    [Name | pg_groups_to_json(Groups)].
