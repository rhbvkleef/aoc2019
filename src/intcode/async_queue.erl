-module(async_queue).

-define(NODEBUG, yes_indeed_dont_show_me_debug_messages_as_i_hate_them).
-include_lib("eunit/include/eunit.hrl").

-export([
  % GenServer
  init/1, handle_call/3, handle_cast/2, start_link/1,
  % Queue operations
  poll/1, push/2
]).

-behaviour(gen_server).
-behaviour(intcode_io).

-type state(T) :: {queue:queue(T), queue:queue(pid())}.

-spec init(_) -> {ok, state(any())}.
init(Name) ->
    case Name of
        nil -> nil;
        Name -> register(Name, self())
    end,
    {ok, {queue:new(), queue:new()}}.

start_link(Name) -> gen_server:start_link(async_queue, Name, []).

procname() ->
    case process_info(self(), registered_name) of
        {registered_name, Name} -> Name;
        _ -> self()
    end.

-spec handle_call(poll, pid(), state(any())) -> any().
handle_call(poll, From, {DataQueue, PollQueue}) ->
    case queue:out(DataQueue) of
        {{value, V}, Q} ->
            ?debugFmt("~w: Polled ~w", [procname(), V]),
            {reply, V, {Q, PollQueue}};
        {empty, Q} ->
            ?debugFmt("~w: Polled (future return)", [procname()]),
            {noreply, {Q, queue:in(From, PollQueue)}}
    end.

-spec handle_cast(any(), state(T)) -> state(T).
handle_cast({push, Value}, {DataQueue, PollQueue}) ->
    case queue:out(PollQueue) of
      {{value, P}, Q} ->
          ?debugFmt("~w: Pushed ~w (direct return)", [procname(), Value]),
          gen_server:reply(P, Value),
          {noreply, {DataQueue, Q}};
      {empty, Q} ->
          ?debugFmt("~w: Pushed ~w (future return)", [procname(), Value]),
          {noreply, {queue:in(Value, DataQueue), PollQueue}}
    end;
handle_cast({push_r, Value}, {DataQueue, PollQueue}) ->
  case queue:out_r(PollQueue) of
    {{value, P}, Q} ->
        ?debugFmt("~w: Pushrd ~w (direct return)", [procname(), Value]),
        gen_server:reply(P, Value),
        {noreply, {DataQueue, Q}};
    {empty, Q} ->
        ?debugFmt("~w: Pushrd ~w (future return)", [procname(), Value]),
        {noreply, {queue:in_r(Value, DataQueue), PollQueue}}
  end.

poll(Ref) -> gen_server:call(Ref, poll).
push(Ref, Value) -> gen_server:cast(Ref, {push, Value}).
