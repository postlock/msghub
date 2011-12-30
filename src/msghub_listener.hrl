-hrl_author('neumark').

-ifndef(MSGHUB_LISTENER_HRL).
-define(MSGHUB_LISTENER_HRL,1).

-record(listener_desc, {
    type,
    options
}).

-record(listener_http_options, {
    port
}).

-endif. %MSGHUB_LISTENER_HRL

