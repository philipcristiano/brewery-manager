%%%-------------------------------------------------------------------
%%% @author Philip Cristiano
%%% @copyright 2015 Philip Cristiano
%%% @doc
%%%
%%% Version1 protocol for interacting with devices
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(bm_protocol_v1).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {socket, device_id}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    io:format("start"),
    {ok, Sock} = gen_tcp:connect({192,168,1,140}, 23, [binary, {packet, line}, {active, true}],30000),

    io:format("connect"),
    %ok = gen_tcp:close(Sock),
    {ok, #state{socket=Sock}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, Port, Msg}, State) ->
    io:format("info ~p~n", [{tcp, Port, Msg}]),
    [Command, Rest] = binary:split(Msg, <<":">>),
    Body = strip(erlang:binary_to_list(Rest), [$\n, $\r]),
    case Command of
        <<"device">> -> handle_proto_device({Port, Body}, State);
        <<"temperature">> -> handle_proto_tempterature({Port, Body}, State);
        _ -> {noreply, State}
    end;
handle_info(Info, State) ->
    io:format("info ~p~n", [Info]),
    {noreply, State}.

handle_proto_device({_Port, Msg}, State) ->
    ID = Msg,
    NewState = State#state{device_id=ID},
    {noreply, NewState}.

handle_proto_tempterature({_Port, Msg}, #state{device_id=ID}=State) ->
    Topic = {bm_temperature, ID},
    [ProbeNum, TempS] = binary:split(Msg),
    pg2:create(Topic),
    Pids = pg2:get_members(Topic),
    send(Pids, temperature, {ProbeNum, TempS}),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
send([], _Name, _Msg) ->
    ok;
send([Pid|Pids], Name, Msg) ->
    Pid ! {pipe, Name, Msg},
    send(Pids, Name, Msg).

strip(S, []) ->
    S;
strip(S, [H|T]) ->
    S1 = string:strip(S, both, H),
    strip(S1, T).
