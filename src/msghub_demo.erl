-module(amqp_example).
-include("amqp_client.hrl").
-compile([export_all]).

amqp_connect() ->
    %% Start a network connection
    {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
    %% Open a channel on the connection
    {ok, Channel} = amqp_connection:open_channel(Connection),
    %% Declare a queue
    #'queue.declare_ok'{queue = Q}
      = amqp_channel:call(Channel, #'queue.declare'{}),
    {Connection, Channel, Queue}.

    %% Publish a message
    Payload = <<"foobar">>,
    Publish = #'basic.publish'{exchange = <<>>, routing_key = Q},
    amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}),
    %% Get the message back from the queue
    Get = #'basic.get'{queue = Q},
    {#'basic.get_ok'{delivery_tag = Tag}, Content}
       = amqp_channel:call(Channel, Get),
    %% Do something with the message payload
    io:format("Get message ~p~n", [Content]),
    %% (some work here)
    %% Ack the message
    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),

amqp_disconnect({Connection, Channel, _}) ->
    %% Close the channel
    amqp_channel:close(Channel),
    %% Close the connection
    amqp_connection:close(Connection),
    ok.

start() ->
    application:start(sockjs),
    application:start(cowboy),
    MQ = aqmp_connect(),
    Port = 9090,
    Dispatch = [{'_', [{'_', sockjs_cowboy_handler,
                        {fun handle/1, fun ws_handle/1}}]}],
    cowboy:start_listener(http, 100,
        cowboy_tcp_transport, [{port,     Port}],
        cowboy_http_protocol, [{dispatch, Dispatch}]),
    io:format("~nRunning on port ~p~n~n", [Port]),
    serve_msghub(start),
    receive
        _ -> ok
    end.

handle(Req) ->
    {Path0, Req1} = sockjs_http:path(Req),
    Path = clean_path(Path0),
    io:format("clean_path returned ~p~n", [Path]),
    case sockjs_filters:handle_req(
           Req1, Path, sockjs_test:dispatcher()) of
        nomatch -> static(Req1, Path);
        Req2    -> Req2
    end.

ws_handle(Req) ->
    {Path0, Req1} = sockjs_http:path(Req),
    Path = clean_path(Path0),
    {Receive, _, _, _} = sockjs_filters:dispatch('GET', Path, dispatcher()),
    {Receive, Req1}.

static(Req, Path) ->
    %% TODO unsafe
    LocalPath = filename:join([module_path(), "priv/www", Path]),
    io:format("LocalPath is ~p~n", [LocalPath]),
    case file:read_file(LocalPath) of
        {ok, Contents} ->
            sockjs_http:reply(200, [], Contents, Req);
        {error, _} ->
            sockjs_http:reply(404, [], "", Req)
    end.

module_path() ->
    {file, Here} = code:is_loaded(?MODULE),
    filename:dirname(filename:dirname(Here)).


clean_path("/")         -> "index.html";
clean_path("/" ++ Path) -> Path.

%% --------------------------------------------------------------------------

dispatcher() ->
    [{msghub,      fun serve_msghub/2}].

serve_msghub(start) ->
    ets:new(msghub_connection_table, [public, named_table]),
    ok.
serve_msghub(Conn, init) ->
    io:format("Incoming socket connection received by pid: ~p~nDetails:~p~n", [self(), Conn]),
    true = ets:insert(msghub_connection_table, {Conn}),
    Gateway = msghub_gateway:start_link(Conn),
    ok;
serve_msghub(Conn, closed) ->
    true = ets:delete_object(msghub_connection_table, {Conn}),
    ok;
serve_msghub(_Conn, {recv, Data}) ->
    io:format("Got data from client: ~p~n", [Data]),
    %ets:foldl(fun({Conn}, _Acc) -> Conn:send(Data) end, [], msghub_connection_table),
    ok.
