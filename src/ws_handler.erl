-module(ws_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

init(Req, State) ->
  io:format(" - websocket connection started: ~p~n", [Req]),
  {cowboy_websocket, Req, State}.

websocket_init(State) ->
  {ok, State}.

websocket_handle({text, Msg}, State) ->
  MsgDecoded = jiffy:decode(Msg),
  {[{<<"msg">>, Command}]} = MsgDecoded,
  case Command of
    <<"greet">> ->
      % start tracing
      trace_all_received_messages(),
      % take a snapshot of registered processes and links
      RegisteredProcessesPids = registered(),
      RunningProcesses = build_processes(RegisteredProcessesPids, []),
      Reply = #{type => <<"greet_back">>, node => node(), processes => RunningProcesses},
      {reply, {text, jiffy:encode(Reply)}, State};
    _ ->
      {ok, State}
  end.

websocket_info({ msg_received, {PidThatReceived, Msg} }, State) ->
  Date = lists:flatten(io_lib:format("~w~2..0w~2..0w", tuple_to_list(date()))),
  Reply = #{type => <<"msg_received">>, from => PidThatReceived, msg => Msg, datetime => Date},
  {reply, {text, jiffy:encode(Reply)}, State};

websocket_info(_Info, State) ->
  {ok, State}.

terminate(_Reason, Req, _State) ->
  io:format(" - websocket connection terminated~n~p~n", [maps:get(peer, Req)]),
  ok.

build_processes([ProcessName|Tail], Nodes) ->
  RegisteredItem = whereis(ProcessName),
  case lists:member(RegisteredItem, erlang:ports()) of
    true ->
      % its a port, ignore this ProcessName
      build_processes(Tail, Nodes);
    _ ->
      % its a process
      Pid = tuple_to_list(pid_tokens(RegisteredItem)),
      {links, Links} = process_info(RegisteredItem, links),
      ParsedLinks = build_links(Links, []),
      % build node
      NewNode = #{
        id => Pid,
        type => case lists:nth(1, Pid) of
          0 -> <<"local">>;
          _ -> <<"remote">>
        end,
        name => ProcessName,
        links => ParsedLinks
      },
      NewNodes = lists:append(Nodes, [NewNode]),
      build_processes(Tail, NewNodes)
  end;
build_processes([], Nodes) -> Nodes.

build_links([LinkName|Tail], Links) ->
  case lists:member(LinkName, erlang:ports()) of
    false ->
      NewLinks = lists:append(Links, [tuple_to_list(pid_tokens(LinkName))]),
      build_links(Tail, NewLinks);
    _ ->
      build_links(Tail, Links)
  end;
build_links([], Links) ->
  Links.

pid_tokens(Pid) ->
  PidStr = pid_to_list(Pid),
  PidStr1 = lists:sublist(PidStr, 2, length(PidStr)-2),
  [N, P1, P2] = [list_to_integer(T) || T <- string:tokens(PidStr1,[$.])],
  {N, P1, P2}.

trace_all_received_messages() ->
  WsPid = self(),
  MyTracerPid = spawn(fun() ->
    receive {trace, P, 'receive', Msg} ->
      PidThatReceived = tuple_to_list(pid_tokens(P)),
      WsPid ! {msg_received, {PidThatReceived, Msg}},
      io:format("this message was received in Pid ~p: ~p~n", [PidThatReceived, Msg])
    end
  end),
  Pid = spawn(fun () -> receive Msg -> io:format("msg received in process: ~p~n", [Msg]) end end),
  erlang:trace(Pid, true, ['receive', {tracer, MyTracerPid}]),
  erlang:trace_pattern({'_', '_', '_'}, [], [local]),
  Pid ! <<"hola">>.
