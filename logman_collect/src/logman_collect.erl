-module(logman_collect).

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

-record(state, {context, sock, ftime, log_system=[], log_resources=[]}).


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
    {ok, FlushTime} = application:get_env(logman, logs_flush_time),
    {ok, Port} = application:get_env(logman, port),
    {ok, Context} = erlzmq:context(),
    {ok, Socket} = erlzmq:socket(Context, [pull, {active, true}]),
    ok = erlzmq:bind(Socket, "tcp://0.0.0.0:" ++ integer_to_list(Port)),
    timer:send_after(FlushTime, ?MODULE, flush_logs),
    {ok, #state{context=Context, sock=Socket, ftime=FlushTime}}.

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
%%

handle_info({zmq, _Socket, Msg, []}, #state{log_system=LogSystem, log_resources=LogResources} = State) ->
    <<LogId:32, LogBin/binary>> = Msg,
    {NewLogSystem, NewLogResources} =
    case LogId of
        1 ->
            {[LogBin | LogSystem], LogResources};
        2 ->
            {LogSystem, [LogBin | LogResources]};
        _ ->
            {LogSystem, LogResources}
    end,
    {noreply, State#state{log_system=NewLogSystem, log_resources=NewLogResources}};

handle_info(flush_logs, #state{ftime=FlushTime, log_system=LogSystem, log_resources=LogResources} = State) ->
    flush_logs([lists:reverse(LogSystem), lists:reverse(LogResources)]),
    timer:send_after(FlushTime, ?MODULE, flush_logs),
    {noreply, State#state{log_system=[], log_resources=[]}};

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
terminate(_Reason, #state{context=Context, sock=Socket} = _State) ->
    erlzmq:close(Socket),
    erlzmq:term(Context),
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
%%%
%%%

flush_logs(Logs) ->
    logman_pool:write_log(Logs).


