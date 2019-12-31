-module(intcode_io_async_queue).

-define(NODEBUG, yes_indeed_dont_show_me_debug_messages_as_i_hate_them).
-include_lib("eunit/include/eunit.hrl").

-export([
  % GenServer
  init/1, handle_call/3, handle_cast/2,

  % IntcodeIO
  poll/1, push/2, poll_or_notify/2, as_list/1,

  % Other
  new/0, new/1, new/2, stop/1
]).

-behaviour(gen_server).
-behaviour(intcode_io).

-record(?MODULE, {
  reference :: reference() | pid()
}).

-record(state, {
  data = intcode_io_queue:new() :: intcode_io_queue:intcode_io_queue(any()),
  listeners = queue:new() :: queue:queue({pid(), term()}),
  name = nil :: any()
}).
-type state(T) :: #state{
  data :: intcode_io_queue:intcode_io_queue(T),
  listeners :: queue:queue({pid(), term()}),
  name :: any()
}.

%% @doc Creates a new empty queue.
new() -> new([]).

%% @doc Creates a new queue with the given items.
new(Items) -> new(Items, nil).

%% @doc Creates a new queue with the given items and name (for debugging).
new(Items, Name) ->
  {ok, Ref} = gen_server:start_link(?MODULE, #state{data = intcode_io_queue:new(Items), name = Name}, []),
  #?MODULE{reference = Ref}.

%% @doc Pops an element from this queue.
%% Defined by the {@link intcode_io} behaviour.
poll(#?MODULE{reference = Ref} = Reference) -> {gen_server:call(Ref, poll), Reference}.

%% @doc Pops an element from this queue.
%% Defined by the {@link intcode_io} behaviour.
poll_or_notify(#?MODULE{reference = Ref} = Reference, Callback) ->
  case gen_server:call(Ref, {poll_notify, Callback}) of
    wait -> {wait, Reference};
    V -> {V, Reference}
  end.

%% @doc Returns the queue contents as a list.
%% Defined by the {@link intcode_io} behaviour.
as_list(#?MODULE{reference = Ref}) -> gen_server:call(Ref, as_list).

%% @doc Pushes an element to this queue.
%% Defined by the {@link intcode_io} behaviour.
push(#?MODULE{reference = Ref} = Reference, Value) -> gen_server:cast(Ref, {push, Value}), Reference.

%% @doc Stop this intcode_io_async_queue.
%% This is normally unnecessary when this process is supervised.
stop(#?MODULE{reference = Ref}) -> gen_server:cast(Ref, stop).

init(State) ->
  {ok, State}.

handle_call(poll, From, #state{data = DataQueue, listeners = PollQueue} = State) ->
  case intcode_io:poll(DataQueue) of
    {V, Q} ->
      ?debugFmt("~w: Polled ~w (immediate return)", [State#state.name, V]),
      {reply, V, State#state{data = Q}};
    R when R == nil orelse R == wait ->
      ?debugFmt("~w: Polled (future return)", [State#state.name]),
      {noreply, State#state{listeners = queue:in(From, PollQueue)}}
  end;
handle_call({poll_notify, Callback}, _, #state{data = DataQueue} = State) ->
  case intcode_io:poll_or_notify(DataQueue, Callback) of
    {R, Q} when R == nil orelse R == wait ->
      ?debugFmt("~w: Pollen (future/callback return)", [State#state.name]),
      {reply, wait, State#state{data = Q}};
    {V, Q} ->
      ?debugFmt("~w: Pollen ~w (immediate return)", [State#state.name, V]),
      {reply, V, State#state{data = Q}}
  end;
handle_call(as_list, _, #state{data = DataQueue} = State) ->
  {reply, intcode_io_queue:as_list(DataQueue), State}.

handle_cast({push, Value}, #state{data = DataQueue, listeners = PollQueue} = State) ->
  case queue:out(PollQueue) of
    {{value, P}, Q} ->
      ?debugFmt("~w: Pushed ~w (direct return)", [State#state.name, Value]),
      gen_server:reply(P, Value),
      {noreply, State#state{listeners = Q}};
    {empty, _} ->
      ?debugFmt("~w: Pushed ~w (future/callback return)", [State#state.name, Value]),
      {noreply, State#state{data = intcode_io:push(DataQueue, Value)}}
  end;
handle_cast(stop, State) -> {stop, normal, State}.
