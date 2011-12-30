-module(msghub_app).
-behaviour(application).
-export([start/2, stop/1]).
-include("msghub_session.hrl").

start(_StartType, _StartArgs) ->
    init_app(),
    msghub_listener_sup:start_link(),
    msghub_session_sup:start_link().

stop(_State) ->
    ok.

% Called when the application starts
init_app() ->
    application:start(sockjs),
    application:start(cowboy),
    ets:new(?ETS, [public, named_table]).
