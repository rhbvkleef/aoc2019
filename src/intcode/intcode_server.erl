%% @doc
%% A VM implementation for the {@link intcode. intcode} instruction set.
-module(intcode_server).

-include("intcode.hrl").

-export([
  % GenServer specifics
  init/1, handle_call/3, handle_cast/2,

  % IntcodeIO implementations
  push/2, poll/1, poll_or_notify/2, as_list/1,

  % Start and control
  start_link/3, new/4, new/3, start/1, stop/1, run_sync/3,

  % Getters
  state/1, output/1, memory/1, 'finished?'/1
]).

-import(intcode, [
  instruction/1,
  increment_pc/3,
  advance/3,
  read_instruction/2
]).

-behaviour(gen_server).
-behaviour(intcode_io).

-record(?MODULE, {
  reference :: pid()
}).

-type ref() :: #?MODULE{reference :: pid()}.
%% Describes the identifier used to reference an instance of the server as
%% defined by this module.

%% @doc Starts a VM node with the provided memory and input and output
%% providers (that implement intcode_io).
-spec start_link(
    Memory :: list(value()),
    Input :: input(),
    Output :: output()) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason :: term()}.
start_link(Memory, Input, Output) -> start_link(Memory, Input, Output, nil, []).

%% @doc Starts a VM node with the provided memory, input, output, name, and
%% gen_server options. The name is used for debugging purposes.
-spec start_link(
    Memory :: list(value()),
    Input :: input(),
    Output :: output(),
    Name :: string() | nil,
    Opts :: gen:options()) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason :: term()}.
start_link([I | _] = Memory, Input, Output, Name, Opts) ->
  MachineState = #machine_state{
    pc = #pc{pc = 0, instruction = I},
    mem = array:from_list(Memory),
    output = Output
  },
  VmState = #vm_state{
    name = Name,
    input = Input,
    input_callback = fun(_, _) -> ok end
  },
  gen_server:start_link(?MODULE, {MachineState, VmState}, Opts).

%% @doc Ensures that `IO' is an {@type intcode_io:intcode_io()}, and if it is a
%% list, converts it to an {@type intcode_io:intcode_io()}.
-spec ensure_intcode_io(list(value()) | intcode_io:intcode_io()) -> intcode_io:intcode_io().
ensure_intcode_io(IO) when is_tuple(IO) andalso is_atom(element(1, IO)) -> IO;
ensure_intcode_io(IO) when is_list(IO) -> intcode_io_queue:new(IO).

%% @doc Creates a new instance of an intcode_io-compatible structure that
%% represents an IntCode VM thread.
-spec new(
    Memory :: list(value()),
    Input :: input() | list(value()),
    Output :: output() | list(),
    Name :: string() | nil) -> ref().
new(Memory, Input, Output, Name) ->
  {ok, Ref} = start_link(Memory, ensure_intcode_io(Input), ensure_intcode_io(Output), Name, []),
  Return = #?MODULE{reference = Ref},
  start(Return),
  Return.

%% @doc Creates a new instance of an intcode_io-compatible structure that
%% represents an IntCode VM thread.
%% This method defaults Name to nil.
-spec new(
    Memory :: list(value()),
    Input :: input() | list(value()),
    Output :: output() | list(value())) -> ref().
new(Memory, Input, Output) -> new(Memory, Input, Output, nil).

%% @doc Return the current machine state (pc, current memory).
-spec state(Reference :: ref()) -> machine_state().
state(#?MODULE{reference = Ref}) -> gen_server:call(Ref, machine_state).

%% @doc Pop one element from the output buffer.
-spec poll(Reference) -> {value(), Reference} when Reference :: ref().
poll(#?MODULE{reference = Ref} = Reference) -> {gen_server:call(Ref, poll), Reference}.

%% @doc Pop one element from the output buffer.
-spec poll_or_notify(Reference, Callback :: fun(() -> any())) -> nil | {value(), Reference} when Reference :: ref().
poll_or_notify(#?MODULE{reference = Ref} = Reference, Callback) ->
  {gen_server:call(Ref, {poll_or_notify, Callback}), Reference}.

%% @deprecated
%% @equiv as_list(Reference)
-spec output(ReferenceOrState :: ref() | machine_state()) -> list(value()).
output(Reference) -> as_list(Reference).

%% @doc Get the output buffer as a list.
-spec as_list(ReferenceOrState :: ref() | machine_state()) -> list(value()).
as_list(#machine_state{output = Output}) -> intcode_io:as_list(Output);
as_list(#?MODULE{} = Reference) -> as_list(state(Reference)).

%% @doc Get the current memory.
-spec memory(ReferenceOrState :: ref() | machine_state()) -> list(value()).
memory(#machine_state{mem = Memory}) -> array:to_list(Memory);
memory(#?MODULE{} = Reference) -> memory(state(Reference)).

%% @doc Wait for the VM to finish.
-spec 'finished?'(Reference :: ref()) -> {ok, normal}.
'finished?'(#?MODULE{reference = Ref}) -> gen_server:call(Ref, wait_finish).

%% @doc Start (or restart) the VM.
-spec start(Reference :: ref()) -> ok.
start(#?MODULE{reference = Ref}) -> gen_server:cast(Ref, start).

%% @doc Shut down the VM.
-spec stop(Reference :: ref()) -> ok.
stop(#?MODULE{reference = Ref}) -> gen_server:cast(Ref, stop).

%% @doc Push a new value onto the output buffer.
-spec push(Reference, Value :: value()) -> Reference when Reference :: ref().
push(#?MODULE{reference = Ref} = Reference, Value) ->
  gen_server:cast(Ref, {input, Value}),
  Reference.

%% @doc Execute the program synchronously
-spec run_sync(
    Program :: list(value()),
    Input :: list(value()) | input(),
    Output :: list(value()) | output()) -> machine_state().
run_sync(Program, Input, Output) ->
  Server = new(Program, Input, Output),
  'finished?'(Server),
  State = state(Server),
  stop(Server),
  State.

%% @doc Initializes this VM.
-spec init(InitialState :: State) -> {ok, State} when State :: {machine_state(), vm_state()}.
init({MachineState, VmState}) ->
  R = self(),
  {ok, {MachineState, VmState#vm_state{input_callback = fun(_, _) -> start(#?MODULE{reference = R}) end}}}.

%% @doc Implement the handle_call function as specified by {@link gen_server}.
%5
%% The @spec'd type is a general type, as `edoc' has issues with rendering very
%% complex typespecs.
%% This is the actual typespec (which is the same as the type specified by the
%% `-spec' annotation on this method.
%% ```
%% (Action, From :: {pid(), Tag :: term()}, State) -> {reply, machine_state(), State}
%%    when Action :: machine_state, State :: {machine_state(), vm_state()};
%% (Action, From :: {pid(), Tag :: term()}, State) -> {reply, value(), State}
%%    when Action :: poll, State :: {machine_state(), vm_state()};
%% (Action, From :: {pid(), Tag :: term()}, State) -> {reply, sleep | value(), State}
%%    when Action :: {poll_or_notify, fun(() -> any())}, State :: {machine_state(), vm_state()};
%% (Action, From :: {pid(), Tag :: term()}, State) -> {reply, list(value()), State}
%%    when Action :: as_list, State :: {machine_state(), vm_state()};
%% (Action, From :: {pid(), Tag :: term()}, State) -> {reply, normal, State} | {noreply, State}
%%    when Action :: wait_finish, State :: {machine_state(), vm_state()}.
%% '''
%% The actual `-spec' typespec can be inspected for this same type.
%%
%% @spec handle_call(Command :: Command, From :: {pid(), Tag :: term()}, State) -> {reply, Reply, State} | {noreply, State}
%%       Command = machine_state | poll | {poll_or_notify, fun(() -> any())} | as_list | wait_finish
%%       State = {machine_state(), vm_state()}
%%       Reply = machine_state() | value() | nil | list(value()) | normal
-spec handle_call
    (Action, From :: {pid(), Tag :: term()}, State) -> {reply, machine_state(), State}
      when Action :: machine_state, State :: {machine_state(), vm_state()};
    (Action, From :: {pid(), Tag :: term()}, State) -> {reply, value(), State}
      when Action :: poll, State :: {machine_state(), vm_state()};
    (Action, From :: {pid(), Tag :: term()}, State) -> {reply, nil | value(), State}
      when Action :: {poll_or_notify, fun(() -> any())}, State :: {machine_state(), vm_state()};
    (Action, From :: {pid(), Tag :: term()}, State) -> {reply, list(value()), State}
      when Action :: as_list, State :: {machine_state(), vm_state()};
    (Action, From :: {pid(), Tag :: term()}, State) -> {reply, normal, State} | {noreply, State}
      when Action :: wait_finish, State :: {machine_state(), vm_state()}.
handle_call(machine_state, _, {MachineState, _} = State) ->
  {reply, MachineState, State};
handle_call(poll, _, {MachineState, RestState}) ->
  {V, Q} = intcode_io:poll(MachineState#machine_state.output),
  {reply, V, {MachineState#machine_state{output = Q}}, RestState};
handle_call({poll_or_notify, Callback}, _, {MachineState, RestState}) ->
  case intcode_io:poll_or_notify(MachineState#machine_state.output, Callback) of
    {R, Q} when R == wait -> {reply, sleep, {MachineState, RestState}};
    {R, Q} -> {reply, R, {MachineState#machine_state{output = Q}, RestState}}
  end;
handle_call(as_list, _, {MachineState, _} = State) ->
  {reply, intcode_io:as_list(MachineState#machine_state.output), State};
handle_call(wait_finish, _, {#machine_state{pc = #pc{instruction = 99}}, _} = State) ->
  {reply, normal, State};
handle_call(wait_finish, From, {MachineState, VmState}) ->
  {noreply, {MachineState, VmState#vm_state{shutdown_listeners = [From | VmState#vm_state.shutdown_listeners]}}}.

%% @doc Implement the handle_cast function as specified by {@link gen_server}.
%5
%% The @spec'd type is a general type, as `edoc' has issues with rendering very
%% complex typespecs.
%% This is the actual typespec (which is the same as the type specified by the
%% `-spec' annotation on this method.
%% ```
%% (Action, State) -> {noreply, State}
%%    when Action :: start | {input, value()} | {set_input, input()}, State :: {machine_state(), vm_state()};
%% (Action, State) -> {stop, normal, State}
%%    when Action :: stop, State :: {machine_state(), vm_state()}.
%% '''
%% The actual `-spec' typespec can be inspected for this same type.
%%
%% @spec handle_cast(Action :: Action, State) -> {noreply, State} | {stop, normal, State}
%%       Action = start | {input, value()} | {set_input, input()} | stop
%%       State = {machine_state(), vm_state()}
-spec handle_cast
    (Action, State) -> {noreply, State}
      when Action :: start | {input, value()} | {set_input, input()}, State :: {machine_state(), vm_state()};
    (Action, State) -> {stop, normal, State}
      when Action :: stop, State :: {machine_state(), vm_state()}.
handle_cast(start, {MachineState, VmState}) ->
  case loop(MachineState, VmState) of
    {halt, NewMachineState, NewVmState} ->
      [gen_server:reply(Listener, normal) || Listener <- NewVmState#vm_state.shutdown_listeners],
      {noreply, {NewMachineState, NewVmState#vm_state{shutdown_listeners = []}}};
    {sleep, NewMachineState, NewVmState} ->
      {noreply, {NewMachineState, NewVmState}}
  end;
handle_cast({input, Value}, {MachineState, VmState}) ->
  NewVmState = VmState#vm_state{input = intcode_io:push(VmState#vm_state.input, Value)},
  {noreply, {MachineState, NewVmState}};
handle_cast({set_input, Input}, {MachineState, VmState}) ->
  {noreply, {MachineState, VmState#vm_state{input = Input}}};
handle_cast(stop, State) ->
  {stop, normal, State}.

%% @doc Runs the interpreter until input is required or the program is
%% finished. In those cases, the new machine state will be `sleep' or `halt'
%% respectively.
-spec loop(machine_state(), vm_state()) -> {sleep | halt, machine_state(), vm_state()}.
loop(#machine_state{pc = Pc, mem = Mem} = MachineState, VmState) ->
  {Arity, Function, Vs} = read_instruction(Pc, Mem),
  case Function(Vs, MachineState, VmState) of
    {continue, NewMachineState, NewVmState} ->
      loop(advance(MachineState, NewMachineState, Arity), NewVmState);
    {sleep, NewMachineState, NewVmState} -> {sleep, advance(MachineState, NewMachineState, Arity), NewVmState};
    {halt, NewMachineState, NewVmState} -> {halt, advance(MachineState, NewMachineState, Arity), NewVmState}
  end.
