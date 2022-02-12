-module(enm_ws_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

init(Req, State) ->
  io:format(" - websocket connection started with state: ~p~n", [State]),
  {cowboy_websocket, Req, State}.

websocket_init(State) ->
  % subscribe this conn pid to to the messages tracer
  enm_messages_tracer ! {subscribe, self()},
  {_, TestProcessPid} = State,
  % test a message
  TestProcessPid ! <<"Hola">>,
  {ok, State}.

% handles commands from the client
websocket_handle({text, Msg}, State) ->
  MsgDecoded = jiffy:decode(Msg),
  {[{<<"msg">>, Command}]} = MsgDecoded,
  case Command of
    <<"greet">> ->
      MyTestProcessPid = whereis(my_test_process),
      MyTestProcessPid ! <<"test message">>,
      % take a snapshot of registered processes and links
      RegisteredProcessesPids = registered(),
      RunningProcesses = build_processes(RegisteredProcessesPids, []),
      Reply = #{type => <<"greet_back">>, node => node(), processes => RunningProcesses},
      {reply, {text, jiffy:encode(Reply)}, State};
    _ ->
      {ok, State}
  end.

% receives a new traced message and pushes data to clients
websocket_info({ msg_received, {PidThatReceived, Msg} }, State) ->
  DateFormatted = enm_util:date_as_string(),
  MsgAsString = enm_util:msg_to_string(Msg),
  Reply = #{type => <<"msg_received">>, at => PidThatReceived, msg => MsgAsString, datetime => DateFormatted},
  {reply, {text, jiffy:encode(Reply)}, State};

% receives a new traced message and pushes data to clients
websocket_info({ msg_sent, {PidThatSent, Msg} }, State) ->
  DateFormatted = enm_util:date_as_string(),
  MsgAsString = enm_util:msg_to_string(Msg),
  Reply = #{type => <<"msg_sent">>, from => PidThatSent, msg => MsgAsString, datetime => DateFormatted},
  {reply, {text, jiffy:encode(Reply)}, State};

websocket_info(_Info, State) ->
  {ok, State}.

terminate(_Reason, Req, _State) ->
  io:format(" - websocket connection terminated~n~p~n", [maps:get(peer, Req)]),
  enm_messages_tracer ! {unsubscribe, self()},
  ok.

% builds a list of processes
build_processes([ProcessName|Tail], Nodes) ->
  RegisteredItem = whereis(ProcessName),
  case lists:member(RegisteredItem, erlang:ports()) of
    true ->
      % its a port, ignore this ProcessName
      build_processes(Tail, Nodes);
    _ ->
      % its a process
      if
        RegisteredItem == undefined ->
          build_processes(Tail, Nodes);
        true ->
          Pid = tuple_to_list(enm_util:pid_tokens(RegisteredItem)),
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
      end
  end;
build_processes([], Nodes) -> Nodes.

% builds a list of process links
build_links([LinkName|Tail], Links) ->
  case lists:member(LinkName, erlang:ports()) of
    false ->
      if
        LinkName == undefined ->
          build_links(Tail, Links);
        true ->
          NewLinks = lists:append(Links, [tuple_to_list(enm_util:pid_tokens(LinkName))]),
          build_links(Tail, NewLinks)
      end;
    _ ->
      build_links(Tail, Links)
  end;
build_links([], Links) ->
  Links.
