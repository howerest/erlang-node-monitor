-module(erlang_node_monitor_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
    {'_', [
			{"/", ws_handler, []}
		]}
  ]),
  {ok, _} = cowboy:start_clear(my_http_listener,
    [{port, 5000}],
    #{env => #{dispatch => Dispatch}}
  ),
  erlang_node_monitor_sup:start_link().

stop(_State) ->
	ok.
