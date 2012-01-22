-module(msghub_sup).
-behaviour(supervisor).
-export([start_link/0]). %% API.
-export([init/1]). %% supervisor.

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link(?MODULE, []).

%% supervisor.
-spec init([]) -> {ok, {{one_for_all, 10, 10}, []}}.
init([]) ->
	{ok, {{one_for_all, 10, 10}, []}}.
