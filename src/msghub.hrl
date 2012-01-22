-hrl_author('neumark').

-ifndef(MSGHUB_LISTENER_HRL).
-define(MSGHUB_LISTENER_HRL,1).

-define(DEFAULT_HTTP_PORT, 9090).
-define(ETS, msghub_sessions_table).

-record(listener_desc, {
    type,
    options
}).

-record(session_desc, {
    id,
    connection,
    user
}).

-endif. %MSGHUB_LISTENER_HRL

