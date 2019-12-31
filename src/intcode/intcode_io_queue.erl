-module(intcode_io_queue).

-behaviour(intcode_io).

-export([
  new/0, new/1,

  % IntcodeIO API
  poll/1, push/2, poll_or_notify/2, as_list/1
]).

-export_type([?MODULE/1]).

-record(?MODULE, {
  queue :: queue:queue(any()),
  listeners = queue:new() :: queue:queue(fun(() -> any()) | fun((?MODULE(any())) -> any()) | pid() | reference())
}).
-type (?MODULE)(T) :: #?MODULE{
  queue :: queue:queue(T),
  listeners :: queue:queue(fun(() -> any()) | fun((?MODULE(any())) -> any()) | pid() | reference())
}.

new() -> new([]).
new(List) when is_list(List) -> #?MODULE{queue = queue:from_list(List)}.

poll(#?MODULE{queue = Q} = State) ->
  case queue:out(Q) of
    {empty, _} -> nil;
    {{value, V}, NewQ} -> {V, State#?MODULE{queue = NewQ}}
  end.

push(#?MODULE{queue = Q, listeners = L} = State, V) ->
  case queue:out(L) of
    {empty, _} ->
      State#?MODULE{queue = queue:in(V, Q)};
    {{value, CB}, NewL} when is_function(CB, 1) ->
      NewState = State#?MODULE{queue = queue:in(V, Q), listeners = NewL},
      CB(NewState),
      NewState;
    {{value, CB}, NewL} ->
      CB(),
      State#?MODULE{queue = queue:in(V, Q), listeners = NewL}
  end.

poll_or_notify(#?MODULE{queue = Q, listeners = L} = State, Callback) ->
  case queue:out(Q) of
    {empty, _} ->
      {wait, State#?MODULE{listeners = queue:in(Callback, L)}};
    {{value, V}, NewQ} -> {V, State#?MODULE{queue = NewQ}}
  end.


as_list(#?MODULE{queue = Q}) -> queue:to_list(Q).
