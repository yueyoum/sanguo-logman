-module(logman_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
%% -define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    LogDBPoolChild = {
     logman_pool, {logman_pool, start_link, []},
     permanent, 2000, worker, [logman_pool]
     },

    LogDBWriterSupervisor = {
     logman_write_sup, {logman_write_sup, start_link, []},
     permanent, 2000, supervisor, [logman_write_sup]
     },

    LogCollectChild = {
     logman_collect, {logman_collect, start_link, []},
     permanent, 2000, worker, [logman_collect]
    },

    Restart = {one_for_one, 5, 10},
    {ok, {Restart, [LogDBPoolChild, LogDBWriterSupervisor, LogCollectChild]}}.

