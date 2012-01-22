-module(msghub_demo).
-include("amqp_client.hrl").
-include("msghub.hrl").
-compile([export_all]).

demo() ->
    application:start(msghub),
    msghub_listener:start(#listener_desc{
        type=http,
        options=[{port, 9090}]
    }).

amqp_connect() ->
    %% Start a network connection
    {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
    %% Open a channel on the connection
    {ok, Channel} = amqp_connection:open_channel(Connection),
    %% Declare a queue
    %#'queue.declare_ok'{queue = Q}
    %  = amqp_channel:call(Channel, #'queue.declare'{}),
    {Connection, Channel}. %, Queue}.

    %% Publish a message
    %Payload = <<"foobar">>,
    %Publish = #'basic.publish'{exchange = <<>>, routing_key = Q},
    %amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}),
    %% Get the message back from the queue
    %Get = #'basic.get'{queue = Q},
    %{#'basic.get_ok'{delivery_tag = Tag}, Content}
    %   = amqp_channel:call(Channel, Get),
    %% Do something with the message payload
    %io:format("Get message ~p~n", [Content]),
    %% (some work here)
    %% Ack the message
    %amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),

amqp_disconnect({Connection, Channel, _}) ->
    %% Close the channel
    amqp_channel:close(Channel),
    %% Close the connection
    amqp_connection:close(Connection),
    ok.

%start() ->
%    application:start(sockjs),
%    application:start(cowboy),
    %MQ = aqmp_connect(),
    %Port = 9090,
    %Dispatch = [{'_', [{'_', sockjs_cowboy_handler,
    %                    {fun handle/1, fun ws_handle/1}}]}],
    %cowboy:start_listener(http, 100,
    %    cowboy_tcp_transport, [{port,     Port}],
    %    cowboy_http_protocol, [{dispatch, Dispatch}]),
    %io:format("~nRunning on port ~p~n~n", [Port]),
    %serve_msghub(start),
    %receive
    %    _ -> ok
    %end.
