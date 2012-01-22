-module(msghub_session).
-compile([export_all]).
-include("msghub.hrl").

make_id(Conn) ->
    Str = lists:flatten(io_lib:format("~p", [[Conn, random:uniform()]])),
    crypto:md5(Str).

dispatch_msg(Conn, init) ->
    SessionDesc = #session_desc{
        id = make_id(Conn),
        connection = Conn,
        user = guest
    },
    true = ets:insert(?ETS, SessionDesc),
    error_logger:info_report(["dispatch_msg init registered session", SessionDesc]),
    ok;

dispatch_msg(Conn, closed) ->
    error_logger:info_report(["dispatch_msg closed called with", {conn, Conn}]),
    %true = ets:delete_object(?ETS, {Conn}),
    ok;

dispatch_msg(Conn, {recv, Data}) ->
    error_logger:info_report(["dispatch_msg recv called with", {conn, Conn}, {data, Data}]),
    ets:foldl(fun(#session_desc{connection=C}, _Acc) -> C:send(Data) end, [], ?ETS),
    ok.

dispatch_manage(Conn, Msg) ->
     error_logger:info_report(["Stub dispatch_manage called with", {conn, Conn}, {msg, Msg}]),
     ok.

