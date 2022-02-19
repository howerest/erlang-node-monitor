-module(erlang_node_monitor_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  erlang_node_monitor_sup:start_link(),
  enm_messages_tracer_sup:start_link(),

  Dispatch = cowboy_router:compile([
    {'_', [
			{"/", enm_ws_handler, []}
		]}
  ]),
  {ok, _} = cowboy:start_clear(my_http_listener,
    [{port, 5000}],
    #{env => #{dispatch => Dispatch}}
  ).

stop(_State) ->
	ok.
