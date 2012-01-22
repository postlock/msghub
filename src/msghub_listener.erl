-module(msghub_listener).
-compile([export_all]).
-include("msghub.hrl").
% We don't need no stinking exports!
%-export([init/0, start_link/2, maybe_create/2, sender/1, reply/2]).
%-export([send/2, close/3]).
%-export([init/1, handle_call/3, handle_info/2, terminate/2, code_change/3, handle_cast/2]).

register_listener(Ref, Desc) ->
    error_logger:info_report(["Stub register_listener called with", {ref, Ref}, {desc, Desc}]).

start(#listener_desc{
            type=Type,
            options=Options
        }=Desc) when Type == http ->
    {Port, Desc1} = case lists:keyfind(port, 1, Options) of
        {port, Portnum} -> {Portnum, Desc};
        false -> {?DEFAULT_HTTP_PORT, Desc#listener_desc{
                options = [{port,?DEFAULT_HTTP_PORT}|Options]
            }}
    end,
    Dispatch = [{'_', [{'_', sockjs_cowboy_handler,
        {fun handle/1, fun ws_handle/1}}]}],
    ListenerRef = cowboy:start_listener(http, 100,
        cowboy_tcp_transport, [{port,     Port}],
        cowboy_http_protocol, [{dispatch, Dispatch}]),
    error_logger:info_report(["Msghub http listener started width options:", Desc1#listener_desc.options]),
    register_listener(ListenerRef, Desc1).

dispatcher() ->
    [{msg,   fun msghub_session:dispatch_msg/2},
     {manage, fun msghub_session:dispatch_manage/2}].

clean_path("/")         -> "index.html";
clean_path("/" ++ Path) -> Path.

handle(Req) ->
    % No static assets are served through cowboy in msghub
    {Path0, Req1} = sockjs_http:path(Req),
    Path = clean_path(Path0),
    io:format("clean_path returned ~p~n", [Path]),
    case sockjs_filters:handle_req(
           Req1, Path, dispatcher()) of
        nomatch ->  sockjs_http:reply(404, [], "", Req1);
        Req2    -> Req2
    end.

ws_handle(Req) ->
    {Path0, Req1} = sockjs_http:path(Req),
    Path = clean_path(Path0),
    {Receive, _, _, _} = sockjs_filters:dispatch('GET', Path, dispatcher()),
    {Receive, Req1}.

