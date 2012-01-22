-module(msghub_app).
-behaviour(application).
-export([start/2, stop/1]).
-include("msghub.hrl").

start(_StartType, _StartArgs) ->
    io:format("In msghub_app:start() at the moment~n", []),
    init_app(),
    msghub_sup:start_link().

stop(_State) ->
    ok.

% Called when the application starts
init_app() ->
    % Start prerequisite applications
    application:start(cowboy),
    application:start(sockjs),
    % create ETS table used to keep track of sessions
    ets:new(?ETS, [public, named_table]).
