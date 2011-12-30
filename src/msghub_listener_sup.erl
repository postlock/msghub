-module(msghub_listener_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/1]).
-export([init/1]).

start_link() ->
     supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 10}, []}}.

start_child(ListenerDesc) ->
    supervisor:start_child(
      ?MODULE,
      {SessionId, {msghub_listener, start_link, [ListenerDesc]},
       transient, 16#ffffffff, worker, [msghub_listener]}).
