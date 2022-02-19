-module(enm_messages_tracer_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  {ok, {{one_for_all, 1, 10}, [ 								% one failure per 10 seconds
		#{id => internal_name,                			% mandatory
	  	start => {enm_messages_tracer, start_link, []}, % mandatory
	  	restart => permanent,               			% optional
	  	shutdown => brutal_kill,                  % optional
	  	type => worker,                     			% optional
	  	modules => [enm_messages_tracer]}         % optional
  ]}}.
