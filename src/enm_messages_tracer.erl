-module(enm_messages_tracer).

-export([start/0]).
-export([my_tracer_loop/1]).
-export([my_test_process_loop/0]).

start() ->
  {MyTracerPid, TestProcessPid} = trace_all_received_messages(),
  TestProcessPid ! <<"hola">>,
  {MyTracerPid, TestProcessPid}.

my_test_process_loop() ->
  receive
    Msg -> io:format("msg received in process: ~p~n", [Msg]),
    my_test_process_loop()
  end.

my_tracer_loop(State) ->
  receive
    % a message received was traced
    {trace, P, 'receive', Msg} ->
      case lists:member(P, erlang:ports()) of
        % ignore ports
        true -> ok;
        _ ->
          % a process
          PidThatReceived = tuple_to_list(enm_util:pid_tokens(P)),
          io:format("this message was traced while received at Pid ~p: ~p~n", [PidThatReceived, Msg]),
          send_to_subscribers(State, {msg_received, {PidThatReceived, Msg}})
      end,
      my_tracer_loop(State);

    {trace, P, 'send', Msg, _} ->
      case lists:member(P, erlang:ports()) of
        % ignore ports
        true -> ok;
        _ ->
          % a process
          PidThatSent = tuple_to_list(enm_util:pid_tokens(P)),
          io:format("this message was traced while sent from Pid ~p: ~p~n", [PidThatSent, Msg]),
          send_to_subscribers(State, {msg_sent, {PidThatSent, Msg}})
      end,
      my_tracer_loop(State);

    % a new ws handler pid subscribes
    {subscribe, Pid} ->
      io:format("a new ws process subscribed ~p~n", [Pid]),
      NewState = lists:append(State, [Pid]),
      my_tracer_loop(NewState);

    {unsubscribe, _} ->
      io:format("a ws process unsubscribed~n"),
      % TODO: remove pid from subscribers
      my_tracer_loop(State);

    Other ->
      io:format("Other msg received at tracer~p~n", [Other])
  end,
  my_tracer_loop(State).

send_to_subscribers([SubscriberPid|Tail], Msg) ->
  SubscriberPid ! Msg,
  send_to_subscribers(Tail, Msg);

send_to_subscribers([], _) ->
  ok.

% traces all messages received at the test process
trace_all_received_messages() ->
  MyTracerPid = spawn(enm_messages_tracer, my_tracer_loop, [[]]),
  register(enm_messages_tracer, MyTracerPid),
  TestProcessPid = spawn(enm_messages_tracer, my_test_process_loop, []),
  register(my_test_process, TestProcessPid),
  erlang:trace(TestProcessPid, true, ['receive', 'send', {tracer, MyTracerPid}]),
  erlang:trace_pattern({'_', '_', '_'}, [], [local]),
  io:format("started tracing messages...~n"),
  {MyTracerPid, TestProcessPid}.
