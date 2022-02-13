-module(enm_util).

-export([pid_tokens/1, date_as_string/0, msg_to_string/1]).

pid_tokens(Pid) ->
  PidStr = pid_to_list(Pid),
  PidStr1 = lists:sublist(PidStr, 2, length(PidStr)-2),
  [N, P1, P2] = [list_to_integer(T) || T <- string:tokens(PidStr1,[$.])],
  {N, P1, P2}.

date_as_string() ->
  {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(erlang:timestamp()),
  Date = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", [Year,Month,Day,Hour,Minute,Second])),
  list_to_binary(Date).

msg_to_string(Msg) ->
  MsgAsString = lists:flatten(io_lib:format("~p", [Msg])),
  list_to_binary(MsgAsString).
