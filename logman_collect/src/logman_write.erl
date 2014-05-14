-module(logman_write).

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

%% -record(state, {}).

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
    gen_server:start_link(?MODULE, [], []).

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
    {ok, []}.

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

handle_cast({write_log, Pool, Logs}, State) ->
    ok = write_log(Pool, Logs),
    {stop, normal, State};

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
%%%
%%%

get_log_system_data(Log) ->
    case Log of
        <<>> ->
            <<>>;
        _ ->
            {ok, {Data}} = msgpack:unpack(Log),
            [
             {<<"char_id">>, CharId},
             {<<"func_name">>, FuncName},
             {<<"occurred_at">>, OccurredAt},
             {<<"msg">>, Msg},
             {<<"error_id">>, ErrorId},
             {<<"levelname">>, LevelName}
            ] = Data,

            XErrorId = integer_to_binary(ErrorId),
            XCharId = integer_to_binary(CharId),
            <<
                $(,
                $', LevelName/binary, $', $,,
                XErrorId/binary, $,,
                XCharId/binary, $,,
                $', FuncName/binary, $', $,,
                $', Msg/binary, $', $,,
                $', OccurredAt/binary, $',
                $)
            >>
    end.

write_log_system(Pool, Logs) ->
    UnpackedLogs = [get_log_system_data(T) || T <- Logs],
    Fun = fun(A, B) ->
            case B of
                <<>> -> A;
                _ -> <<A/binary, $,, B/binary>>
            end
          end,

    Values = lists:foldr(Fun, <<>>, UnpackedLogs),

    Header = <<"INSERT INTO log_system (levelname, error_id, char_id, func_name, msg, occurred_at) VALUES ">>,
    SQL = <<Header/binary, Values/binary>>,

    _Result = emysql:execute(Pool, SQL),
    ok.



get_log_resources_data(Log) ->
    case Log of
        <<>> ->
            <<>>;
        _ ->
            {ok, {Data}} = msgpack:unpack(Log),
            [
                {<<"exp">>, Exp},
                {<<"gold">>, Gold},
                {<<"func_name">>, FuncName},
                {<<"occurred_at">>, OccurredAt},
                {<<"souls">>, Souls},
                {<<"equipments">>, Equipments},
                {<<"stuffs">>, Stuffs},
                {<<"gems">>, Gems},
                {<<"char_id">>, CharId},
                {<<"sycee">>, Sycee},
                {<<"des">>, Des},
                {<<"heros">>, Heros},
                {<<"income">>, Income},
                {<<"official_exp">>, OfficialExp}
            ] = Data,

            IntExp = integer_to_binary(Exp),
            IntGold = integer_to_binary(Gold),
            IntCharId = integer_to_binary(CharId),
            IntSycee = integer_to_binary(Sycee),
            IntIncome = integer_to_binary(Income),
            IntOfficialExp = integer_to_binary(OfficialExp),

            <<
                $(,
                IntCharId/binary, $,,
                IntIncome/binary, $,,
                $', FuncName/binary, $', $,,
                IntExp/binary, $,,
                IntOfficialExp/binary, $,,
                IntGold/binary, $,,
                IntSycee/binary, $,,
                $', Heros/binary, $', $,,
                $', Souls/binary, $', $,,
                $', Equipments/binary, $', $,,
                $', Gems/binary, $', $,,
                $', Stuffs/binary, $', $,,
                $', Des/binary, $', $,,
                $', OccurredAt/binary, $',
                $)
            >>
    end.

write_log_resources(Pool, Logs) ->
    UnpackedLogs = [get_log_resources_data(T) || T <- Logs],
    Fun = fun(A, B) ->
            case B of
                <<>> -> A;
                _ -> <<A/binary, $,, B/binary>>
            end
          end,

    Values = lists:foldr(Fun, <<>>, UnpackedLogs),

    Header = <<"INSERT INTO log_resource (char_id, income, func_name, exp, official_exp, gold, sycee, heros, souls, equipments, gems, stuffs, des, occurred_at) VALUES ">>,
    SQL = <<Header/binary, Values/binary>>,
    io:format("~p~n", [SQL]),

    _Result = emysql:execute(Pool, SQL),
    io:format("~p~n", [_Result]),
    ok.


write_log(Pool, [LogSystem, LogResources]) ->
    case LogSystem of
        [] ->
            ok;
        LogSystem ->
            write_log_system(Pool, LogSystem)
    end,

    case LogResources of
        [] ->
            ok;
        LogResources ->
            write_log_resources(Pool, LogResources)
    end,

    ok.

