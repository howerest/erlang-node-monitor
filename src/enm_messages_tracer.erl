-module(enm_messages_tracer).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).
-export([trace_all_received_messages/1]).
-export([my_test_process_loop/0]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% gen_server.

init([]) ->
	MyTracerPid = self(),
	register(enm_messages_tracer, MyTracerPid),
	my_test_process = trace_all_received_messages(MyTracerPid),
  my_test_process ! <<"hola">>,
	{ok, []}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

% a message received was traced
handle_info({ trace, P, 'receive', Msg }, State) ->
	case lists:member(P, erlang:ports()) of
		% ignore ports
		true -> ok;
		_ ->
			% a process
			PidThatReceived = list_to_binary(lists:flatten(io_lib:format("~p", [P]))),
			% PidThatReceived = pid_to_list(P),
			% io:format("this message was traced while received at Pid ~p: ~p~n", [PidThatReceived, Msg]),
			send_to_subscribers(State, {msg_received, {PidThatReceived, Msg}})
	end,
	{noreply, State};

% a message sent was traced
handle_info({ trace, P, 'send', Msg, _ }, State) ->
	case lists:member(P, erlang:ports()) of
		% ignore ports
		true -> ok;
		_ ->
			% a process
			% PidThatSent = pid_to_list(P),
			PidThatSent = list_to_binary(lists:flatten(io_lib:format("~p", [P]))),
			% io:format("this message was traced while sent from Pid ~p: ~p~n", [PidThatSent, Msg]),
			send_to_subscribers(State, {msg_sent, {PidThatSent, Msg}})
	end,
	{noreply, State};

% a spawned process was traced
handle_info({ trace, ParentPid, 'spawned', SpawnedPid, _ }, State) ->
	ParentPidFormatted = enm_util:pid_to_string(ParentPid),
	SpawnedPidFormatted = enm_util:pid_to_string(SpawnedPid),
	send_to_subscribers(State, {new_process, {ParentPidFormatted, SpawnedPidFormatted}}),
	{noreply, State};

% a terminated process was traced
handle_info({ trace, DiedPid, 'exit', Reason }, State) ->
	DiedPidFormatted = enm_util:pid_to_string(DiedPid),
	send_to_subscribers(State, {end_process, {DiedPidFormatted, Reason}}),
	{noreply, State};

handle_info(_, State) ->
	{noreply, State}.

% a new ws pid subscribes
handle_cast({ subscribe, Pid }, State) ->
	io:format("a ws process subscribed~n"),
	NewState = lists:append(State, [Pid]),
	io:format("ws subscribed: ~p~n", [NewState]),
	{noreply, NewState};

% a ws pid unsubscribes
handle_cast({ unsubscribe, Pid }, State) ->
	io:format("a ws process unsubscribed~n"),
	NewState = lists:filter(fun (Elem) -> Elem =:= Pid end, State),
	io:format("ws subscribed: ~p~n", [NewState]),
	{noreply, NewState};

handle_cast(_, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% --------------------------------------------------------------

send_to_subscribers([SubscriberPid|Tail], Msg) ->
  % io:format("Sending to: ~p : ~p~n", [SubscriberPid, Msg]),
  SubscriberPid ! Msg,
  send_to_subscribers(Tail, Msg);

send_to_subscribers([], _) ->
  ok.

% traces all messages received at the test process
trace_all_received_messages(MyTracerPid) ->
  TestProcessPid = spawn(enm_messages_tracer, my_test_process_loop, []),
  WhereIs = whereis(my_test_process),
  if
    WhereIs == undefined ->
      register(my_test_process, TestProcessPid);
    true ->
      unregister(my_test_process),
      register(my_test_process, TestProcessPid)
  end,
  erlang:trace(all, true, ['receive', 'send', 'procs', {tracer, MyTracerPid}]),
  erlang:trace_pattern({'_', '_', '_'}, [], [local]),
  io:format("started tracing messages...~n"),
  my_test_process.


my_test_process_loop() ->
  receive
    Msg -> io:format("msg received in process: ~p~n", [Msg]),
    my_test_process_loop()
  end.
