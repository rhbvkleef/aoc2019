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

-import(intcode, [instruction/1]).

-behaviour(gen_server).
-behaviour(intcode_io).

-record(?MODULE, {
  reference :: {pid(), term()}
}).

%% @doc Starts a VM node with the provided memory and input and output
%% providers (that implement intcode_io).
-spec start_link(memory(), input(), output()) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason :: term()}.
start_link(Memory, Input, Output) -> start_link(Memory, Input, Output, nil, []).

start_link([I|_] = Memory, Input, Output, Name, Opts) ->
  MachineState = #machine_state{
    pc = #pc{pc = 0, instruction = I},
    mem = array:from_list(Memory),
    output = Output
  },
  VmState = #vm_state{
    name = Name,
    input = Input
  },
  gen_server:start_link(?MODULE, {MachineState, VmState}, Opts).

ensure_intcode_io(IO) when is_list(IO) -> intcode_io_queue:new(IO);
ensure_intcode_io(IO) -> IO.

%% @doc Creates a new instance of an intcode_io-compatible structure that
%% represents an IntCode VM thread.
new(Memory, Input, Output, Name) ->
  {ok, Ref} = start_link(Memory, ensure_intcode_io(Input), ensure_intcode_io(Output), Name, []),
  Return = #?MODULE{reference = Ref},
  start(Return),
  Return.
%% @doc Creates a new instance of an intcode_io-compatible structure that
%% represents an IntCode VM thread.
%% This method defaults Name to nil.
new(Memory, Input, Output) -> new(Memory, Input, Output, nil).

%% @doc Return the current machine state (pc, current memory).
state(#?MODULE{reference = Ref}) -> gen_server:call(Ref, machine_state).

%% @doc Pop one element from the output buffer.
poll(#?MODULE{reference = Ref} = Reference) -> {gen_server:call(Ref, poll), Reference}.

%% @doc Pop one element from the output buffer.
poll_or_notify(#?MODULE{reference = Ref} = Reference, Callback) ->
  case gen_server:call(Ref, {poll_or_notify, Callback}) of
    nil -> nil;
    V -> {V, Reference}
  end.

%% @doc Get the output buffer as a list.
%% @deprecated
output(Reference) -> as_list(Reference).

%% @doc Get the output buffer as a list.
as_list(#machine_state{output = Output}) -> intcode_io:as_list(Output);
as_list(#?MODULE{reference = Ref}) -> as_list(state(Ref)).

%% @doc Get the current memory.
memory(#machine_state{mem = Memory}) -> array:to_list(Memory);
memory(#?MODULE{reference = Ref}) -> memory(state(Ref)).

%% @doc Wait for the VM to finish.
'finished?'(#?MODULE{reference = Ref}) -> gen_server:call(Ref, wait_finish).

%% @doc Start (or restart) the VM.
start(#?MODULE{reference = Ref}) -> gen_server:cast(Ref, start).

%% @doc Shut down the VM.
stop(#?MODULE{reference = Ref}) -> gen_server:cast(Ref, stop).

%% @doc Push a new value onto the output buffer.
push(#?MODULE{reference = Ref} = Reference, Value) ->
  gen_server:cast(Ref, {input, Value}),
  Reference.

%% @doc Execute the program synchronously
run_sync(Program, Input, Output) ->
  Server = new(Program, Input, Output),
  'finished?'(Server),
  State = state(Server),
  stop(Server),
  State.

init({MachineState, VmState}) ->
  R = self(),
  {ok, {MachineState, VmState#vm_state{input_callback = fun (_, _) -> start(#?MODULE{reference = R}) end}}}.

handle_call(machine_state, _, {MachineState, _} = State) ->
  {reply, MachineState, State};
handle_call(poll, _, {MachineState, RestState}) ->
  {V, Q} = intcode_io:poll(MachineState#machine_state.output),
  {reply, V, {MachineState#machine_state{output = Q}}, RestState};
handle_call({poll_or_notify, Callback}, _, {MachineState, RestState}) ->
  case intcode_io:poll_or_notify(MachineState#machine_state.output, Callback) of
    nil -> {reply, nil, {MachineState, RestState}};
    {R, Q} -> {reply, R, {MachineState#machine_state{output = Q}, RestState}}
  end;
handle_call(as_list, _, {MachineState, _} = State) ->
  {reply, intcode_io:as_list(MachineState#machine_state.output), State};
handle_call(wait_finish, _, {#machine_state{pc = #pc{instruction = 99}}, _} = State) ->
  {reply, normal, State};
handle_call(wait_finish, From, {MachineState, VmState}) ->
  {noreply, {MachineState, VmState#vm_state{shutdown_listeners = [From | VmState#vm_state.shutdown_listeners]}}}.

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
handle_cast(stop, {_, VmState} = State) ->
  {stop, normal, State}.

loop(#machine_state{pc = Pc, mem = Mem} = MachineState, VmState) ->
  {Arity, Function, Vs} = read_instruction(Pc, Mem),
  case Function(Vs, MachineState, VmState) of
    {continue, NewMachineState, NewVmState} ->
      #machine_state{mem = NM} = NewNewMachineState = advance(MachineState, NewMachineState, Arity),
      loop(NewNewMachineState, NewVmState);
    {sleep, NewMachineState, NewVmState} -> {sleep, advance(MachineState, NewMachineState, Arity), NewVmState};
    {halt, NewMachineState, NewVmState} -> {halt, advance(MachineState, NewMachineState, Arity), NewVmState}
  end.

% For some reason, the length of the Modes array will be one longer (too long?) than lists:seq(1, Arity),
% and I am not sure, so I just pad it to one less.
read_instruction(#pc{pc = Pos, instruction = I}, Memory) ->
  {Arity, Function} = instruction(I rem 100),
  XModes = lists:reverse(
    lists:sublist(
      integer_to_list(I),
      max(0, length(integer_to_list(I)) - 2))
  ),
  Modes = XModes ++ [$0 || _ <- lists:seq(1, Arity - length(XModes))],
  Vs = [
    case M of
      $1 -> {array:get(O + Pos, Memory), array:get(O + Pos, Memory)};
      _ -> {array:get(O + Pos, Memory), array:get(array:get(O + Pos, Memory), Memory)}
    end || {M, O} <- lists:zip(Modes, lists:seq(1, Arity))],
  {Arity, Function, Vs}.

advance(#machine_state{pc = OldPc, mem = OldMem, output = OldOutp}, #machine_state{pc = NewPc, mem = NewMem, output = NewOutp}, Arity) ->
  Pc = case {NewPc, NewMem} of
         {nil, nil} -> increment_pc(OldPc, OldMem, Arity);
         {nil, _} -> increment_pc(OldPc, NewMem, Arity);
         _ -> NewPc
       end,
  Mem = case NewMem of nil -> OldMem; _ -> NewMem end,
  Outp = case NewOutp of nil -> OldOutp; _ -> NewOutp end,
  #machine_state{
    pc = Pc,
    mem = Mem,
    output = Outp
  }.

-spec increment_pc(pc(), memory(), instruction_arity()) -> pc().
increment_pc(#pc{pc = I}, Memory, Arity) ->
  #pc{pc = I + Arity + 1, instruction = array:get(I + Arity + 1, Memory)}.
