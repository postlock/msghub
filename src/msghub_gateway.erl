-module(msghub_gateway).
-behaviour(gen_server).
-include("amqp_client.hrl").
-compile([export_all]).


-record(state, {
    MQ_conn
}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    % Don't die on 'EXIT' signal
    %process_flag(trap_exit, true),
    State0 = #state{
        MQ_conn = connect_to_MQ(),
    },
    {ok, State0}. 

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

%% Handle incoming connection requests from new pariticpants.
handle_call({connect_client, Connection}, _From, State) ->
    ParticipantId = State#state.next_participant_id,
    {Reply, NewState} = case plGateway:start_link([
        self(), State#state.application_process, ParticipantId, Connection]) of
        {ok, Gateway} -> 
            NS = add_participant(State, #pl_participant{
                % No username yet, that comes after authentication.
                id = ParticipantId,
                process_id = Gateway,
                status = connected
            }),
            % update next participant id
            NS1 = NS#state{next_participant_id = ParticipantId + 1},
            WSOwner = gen_fsm:sync_send_event(Gateway, {get_websocket_owner}),
            {{ok, {websocket, WSOwner, passive}},
              NS1};
        Err = {error, _Reason} ->
            {Err, State}
    end,
    {reply, Reply, NewState};
%% Returns the application server. 
handle_call({get_application_process}, _From, State) ->
   {reply, State#state.application_process, State};

%% Returns session id to caller.
handle_call({get_session_id}, _From, #state{session_id=Sid}=State) ->
   {reply, Sid, State};

handle_call(Request, From, State) ->
    error_logger:warning_report(["plSession:handle_call/3: unhandled message",
        {request, Request},
        {from, From},
        {state, State}]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% Update participant ID.
handle_cast({update_participant_data, PData}, 
    #state{participants = P} = State) ->
    % Only username update is currently supported
    % TODO: check for case when Id doesnt refer to valid participant.
    {id, Id} = lists:keyfind(id, 1, PData),
    {username, Username} = lists:keyfind(username, 1, PData),
    % TODO: handle case in which lookup returns none
    {value, Participant} = gb_trees:lookup(Id, P),
    NewState = State#state{participants = 
                    gb_trees:update(Id,Participant#pl_participant{
                        username=Username,
                        status=authenticated
                    },P)}, 
    {noreply, NewState};

%% Called when a client disconnects.
handle_cast(
    {disconnect, 
        {ParticipantId,
         Reason,
         Details}}, 
    #state{participants = Participants} = State) ->
    NewState = case gb_trees:lookup(ParticipantId, Participants) of
        % update participant status to 'disconnecting'
        {value, Participant}->
            State#state{participants = gb_trees:update(ParticipantId,
                Participant#pl_participant{
                    status = disconnecting,
                    participant_data = {Reason, Details}
                }, Participants)};
        none ->
            error_logger:error_report(["disconnect message received from nonexisting participant"]), 
            State
    end,
    {noreply, NewState};

%% Delivers messages between participants
handle_cast({deliver_message, #pl_client_msg{to=To}=Msg}, 
    #state{participants=Participants}=State) ->
    case To of 
        ?SESSION_SERVER_PARTICIPANT_ID ->
            io:format("Session server got message ~p~n", [Msg]);
        _OtherParticipant ->
            % TODO: send error if deliver_message returns {error, _}
            deliver_message(Msg, Participants)
    end,
    {noreply, State};
%% This is a little hacky...
handle_cast({broadcast, Msg}, 
    #state{participants=Participants}=State) ->
    [begin
        {_, Participant} = P,
        To = Participant#pl_participant.id,
        case To > 1 of
            false -> noop;
            true -> deliver_message(Msg#pl_client_msg{to=To}, Participants)
        end
    end || P <- gb_trees:to_list(Participants)],
    {noreply, State};
 
 
handle_cast(Msg, State) ->
    error_logger:warning_report(["plSession:handle_cast/2: unhandled message",
        {message, Msg},
        {state, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, State) ->
    {noreply, on_exit(Pid, Reason, State)};

handle_info(Info, State) ->
    error_logger:warning_report(["plSession:handle_info/2: unhandled message",
        {message, Info},
        {state, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%%  Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: add_system_participants(#state) -> {ok, Participants} |
%%                         {error, Reason} |
%% Description: spawns the postlock 'system participants', which are
%% the mandatory participants for any functioning session.
%% These are:
%%   Session Server (this process)
%%   State Server (lives in plState, spawned here)
%%--------------------------------------------------------------------
connect_to_MQ() ->
    %% Start a network connection
    {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
    %% Open a channel on the connection
    {ok, Channel} = amqp_connection:open_channel(Connection),
    {Connection, Channel}.

disconnect_from_MQ({Connection, Channel}) ->
    %% Close the channel
    amqp_channel:close(Channel),
    %% Close the connection
    amqp_connection:close(Connection),
    ok.


