%%%-------------------------------------------------------------------
%%% @author Philip Cristiano
%%% @copyright 2014 Philip Cristiano
%%% @doc
%%% gen_server that manages the TSDB
%%% @end
%%%-------------------------------------------------------------------

-module(bm_tsdb_server).

-behaviour(gen_server).

%% API
-export([start_link/0,
         aggregate/4,
         data/1,
         metrics/0,
         write/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {dbref}).

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
    {ok, DBRef} = leveltsdb:open("db"),
    {ok, #state{dbref=DBRef}}.


%% API
write(Metric, TS, Value) ->
    gen_server:call(?MODULE, {write, Metric, TS, Value}).

metrics() ->
    gen_server:call(?MODULE, metrics).

data(Metric) ->
    gen_server:call(?MODULE, {data, Metric}).

aggregate(Metric, TS1, TS2, Opts) ->
    gen_server:call(?MODULE, {aggregate, Metric, TS1, TS2, Opts}).


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
handle_call({aggregate, Metric, TS1, TS2, Opts}, _From, State=#state{dbref=DBRef}) ->
    {ok, Data} = leveltsdb:aggregate(DBRef, Metric, TS1, TS2, <<"avg">>, [{aggregation, <<"avg">>}| Opts]),
    {reply, {ok, Data}, State};
handle_call(metrics, _From, State=#state{dbref=DBRef}) ->
    {ok, Metrics} = leveltsdb:metrics(DBRef),
    {reply, {ok, Metrics}, State};
handle_call({data, Metric}, _From, State=#state{dbref=DBRef}) ->
    Acc = leveltsdb:fold_metric(DBRef, Metric, fun accumulate/2, []),
    RAcc = lists:reverse(Acc),
    {reply, {ok, RAcc}, State};
handle_call({write, Metric, TS, Value}, _From, State=#state{dbref=DBRef}) ->
    io:format("Got one! ~p~n", [{Metric, TS, Value}]),
    ok = leveltsdb:write(DBRef, Metric, TS, Value),
    {reply, ok, State};
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
handle_info(_Info, State) ->
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

accumulate({TS, Value}, Acc) ->
    [{TS, Value} | Acc].
