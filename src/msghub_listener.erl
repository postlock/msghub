-module(msghub_listener).
-behaviour(gen_server).
-compile([export_all]).
-include("msghub_listener.hrl").
% We don't need no stinking exports!
%-export([init/0, start_link/2, maybe_create/2, sender/1, reply/2]).
%-export([send/2, close/3]).
%-export([init/1, handle_call/3, handle_info/2, terminate/2, code_change/3, handle_cast/2]).

-record(state, {
    listener_pid,
    listener_state
}).

start_link(ListenerDesc) ->
    gen_server:start_link(?MODULE, ListenerType, []).

init(ListenerType) ->
    %ets:insert(?ETS, {SessionId, self()}),
    %process_flag(trap_exit, true),
    Listener = start_listener(ListenerType),
    {ok, #state{
        listener_pid = Listener,
        listener_state = running
    }}.

handle_call(Request, _From, State) ->
    {stop, {odd_request, Request}, State}.

handle_cast(Cast, State) ->
    {stop, {odd_cast, Cast}, State}.

handle_info(Info, State) ->
    {noreply, State}.

%handle_info({'EXIT', Pid, _Reason},
%            State = #session{response_pid = Pid}) ->
%    {ok, CloseTime} = application:get_env(sockjs, session_close_ms),
%    Ref = erlang:send_after(CloseTime, self(), session_timeout),
%    {noreply, State#session{response_pid    = undefined,
                            session_timeout = Ref}};

terminate(_Reason, _State) ->
    %Receive({?MODULE, SessionId}, closed),
    %ets:delete(?ETS, SessionId),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%-----------------
start_listener({
        listener_desc,
        http, 
        #listener_http_options {
            port=Port
        }}) ->
    Dispatch = [{'_', [{'_', sockjs_cowboy_handler,
                        {fun handle/1, fun ws_handle/1}}]}],
    cowboy:start_listener(http, 100,
        cowboy_tcp_transport, [{port,     Port}],
        cowboy_http_protocol, [{dispatch, Dispatch}]),
    error_logger:info_report(["Msghub http listener started", {port,Port}]).
    serve_msghub(start),
    receive
        _ -> ok
    end.

