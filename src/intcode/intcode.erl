%% @doc
%% This module defines `intcode' instruction set along with providing tools to read `intcode' programs from a string.
-module(intcode).

-include_lib("eunit/include/eunit.hrl").

-include("intcode.hrl").

-export([
  % Actual public API
  read_program/1,

  % API for VM implementations
  instruction/1,
  increment_pc/3,
  advance/3,
  read_instruction/1
]).

-export_type([
  memory/0, input/0, output/0, value/0, address/0, instruction/0
]).

%% @doc
%% Reads an intcode program from a single unquoted CSV line.
-spec read_program(Line :: string()) -> list(value()).
read_program(Line) ->
  [list_to_integer(I) || I <- string:tokens(Line, ",")].

%% @doc
%% Defines the `intcode' instruction set.
%%
%% This function is intended to be used only by VM implementations.
%%
%% <table>
%%   <thead>
%%     <tr>
%%       <th>Code</th>
%%       <th>Instruction name</th>
%%       <th>Description</th>
%%     </tr>
%%   </thead>
%%   <tbody>
%%     <tr>
%%       <td>1</td>
%%       <td>ADD</td>
%%       <td>Set `#C=A+B'</td>
%%     </tr>
%%     <tr>
%%       <td>2</td>
%%       <td>MUL</td>
%%       <td>Set `#C=A*B'.</td>
%%     </tr>
%%     <tr>
%%       <td>3</td>
%%       <td>INP</td>
%%       <td>Read from input and store to `#A'</td>
%%     </tr>
%%     <tr>
%%       <td>4</td>
%%       <td>OUT</td>
%%       <td>Write `A' to output</td>
%%     </tr>
%%     <tr>
%%       <td>5</td>
%%       <td>JNZ</td>
%%       <td>Set `PC=#B' when `A=/=0'</td>
%%     </tr>
%%     <tr>
%%       <td>6</td>
%%       <td>JZ</td>
%%       <td>Set `PC=#B' when `A==0'</td>
%%     </tr>
%%     <tr>
%%       <td>7</td>
%%       <td>TLT</td>
%%       <td>Set `#C=1' when `A<B', otherwise set `#C=0'</td>
%%     </tr>
%%     <tr>
%%       <td>8</td>
%%       <td>TEQ</td>
%%       <td>Set `#C=1' when `A==B', otherwise set `#C=0'</td>
%%     </tr>
%%     <tr>
%%       <td>99</td>
%%       <td>HLT</td>
%%       <td>Halt the VM</td>
%%     </tr>
%%   </tbody>
%% </table>
%% @param Code The instruction to look up
%% @returns An instruction representation as defined by the {@type instruction()} type.
-spec instruction(Code :: value()) -> instruction().
instruction(1) -> alu(fun(A, B) -> A + B end);
instruction(2) -> alu(fun(A, B) -> A * B end);
instruction(3) -> {1, fun input/3};
instruction(4) -> {1, fun output/3};
instruction(5) -> jumpwhen(fun(A) -> A =/= 0 end);
instruction(6) -> jumpwhen(fun(A) -> A == 0 end);
instruction(7) -> alu(fun(A, B) when A < B -> 1; (_, _) -> 0 end);
instruction(8) -> alu(fun(A, B) when A == B -> 1; (_, _) -> 0 end);
instruction(9) -> {1, fun([{_, A}], #machine_state{relbase = RB}, VmState) -> {continue, #machine_state{relbase = A + RB}, VmState} end};
instruction(99) -> {0, fun(_, #machine_state{pc = Pc}, VmState) -> {halt, #machine_state{pc = Pc}, VmState} end}.

%% @doc Increments the program counter to the next instruction.
%%
%% This function is intended to be used only by VM implementations.
-spec increment_pc(pc(), memory(), instruction_arity()) -> pc().
increment_pc(#pc{pc = I}, Memory, Arity) ->
  #pc{pc = I + Arity + 1, instruction = array:get(I + Arity + 1, Memory)}.

%% @doc Updates `OldMachineState' with the deltas provided in
%% `NewMachineStateDelta' and optionally advances the program counter with
%% `Arity'.
%%
%% This function is intended to be used only by VM implementations.
-spec advance(OldMachineState :: machine_state(), NewMachineStateDelta :: partial_machine_state(), Arity :: non_neg_integer()) -> machine_state().
advance(#machine_state{pc = OldPc, mem = OldMem, output = OldOutp, relbase = OldRelBase}, #machine_state{pc = NewPc, mem = NewMem, output = NewOutp, relbase = NewRelBase}, Arity) ->
  Pc = case {NewPc, NewMem} of
         {nil, nil} -> increment_pc(OldPc, OldMem, Arity);
         {nil, _} -> increment_pc(OldPc, NewMem, Arity);
         _ -> NewPc
       end,
  Mem = case NewMem of nil -> OldMem; _ -> NewMem end,
  Outp = case NewOutp of nil -> OldOutp; _ -> NewOutp end,
  RelBase = case NewRelBase of nil -> OldRelBase; _ -> NewRelBase end,
  #machine_state{
    pc = Pc,
    mem = Mem,
    output = Outp,
    relbase = RelBase
  }.

%% @doc Decodes the instruction in the given program counter.
%%
%% This function is intended to be used only by VM implementations.
%%
%% For some reason, the length of the Modes array will be one longer (too long?) than lists:seq(1, Arity),
%% and I am not sure, so I just pad it to one less.
-spec read_instruction(machine_state()) ->
  {
    Arity :: non_neg_integer(),
    Function :: fun((list(instruction_argument()), machine_state(), vm_state()) -> {continuation_method(), partial_machine_state(), vm_state()}),
    InstructionArguments :: list(instruction_argument())
  }.
read_instruction(#machine_state{pc=#pc{pc = Pos, instruction = I}, mem=Memory, relbase = Relbase}) ->
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
      $2 -> {array:get(O + Pos, Memory) + Relbase, array:get(array:get(O + Pos, Memory) + Relbase, Memory)};
      _ -> {array:get(O + Pos, Memory), array:get(array:get(O + Pos, Memory), Memory)}
    end || {M, O} <- lists:zip(Modes, lists:seq(1, Arity))],
  {Arity, Function, Vs}.

%% @doc Sets the value at the given address in memory.
-spec memset(address(), value(), memory()) -> memory().
memset(Addr, Value, Mem) -> array:set(Addr, Value, Mem).

%% @doc Returns the value at the given address in memory.
-spec memget(address(), memory()) -> value().
memget(Addr, Mem) -> case Addr >= array:size(Mem) of true -> 0; _ -> array:get(Addr, Mem) end.

%% @doc Returns an instruction specification for an ALU-like operation.
-spec alu(fun((value(), value()) -> value())) -> instruction().
alu(Fun) ->
  {3,
    fun([{_, A}, {_, B}, {Coff, _}], #machine_state{mem = Memory}, VmState) ->
      {continue, #machine_state{
        mem = memset(Coff, Fun(A, B), Memory)
      }, VmState}
    end
  }.

%% @doc Reads a value from the input to the address specified by the first
%% argument to this function.
-spec input(
    InstructionArguments :: list(instruction_argument()),
    CurrentMachineState :: machine_state(),
    CurrentVmState :: vm_state()) ->
  {ContinuationMethod :: continuation_method(), NewMachineStateDelta :: partial_machine_state(), NewVmState :: vm_state()}.
input([{Aoff, _}], #machine_state{mem = Memory} = MachineState, VmState) ->
  case read_input(MachineState, VmState) of
    {ok, X, NewVmState} -> {continue,
      #machine_state{
        mem = memset(Aoff, X, Memory)
      },
      NewVmState
    };
    {sleep, NewVmState} -> {sleep,
      MachineState,
      NewVmState
    }
  end.

%% @doc Outputs the value of the first argument to this function.
-spec output(
    InstructionArguments :: list(instruction_argument()),
    CurrentMachineState :: machine_state(),
    CurrentVmState :: VmState) ->
  {continuation_method(), NewMachineStateDeltas :: partial_machine_state(), NewVmState :: VmState}
  when VmState :: vm_state().
output([{_, A}], MachineState, VmState) ->
  {continue,
    #machine_state{
      output = write_output(MachineState#machine_state.output, A)
    },
    VmState
  }.

%% @doc Jumps when `Fun' returns ``'true' '' for the value of the first
%% argument to this instruction.
-spec jumpwhen(fun((value()) -> boolean())) -> instruction().
jumpwhen(Fun) -> {2,
  fun([{_, A}, {_, B}], #machine_state{mem = Memory}, VmState) ->
    case Fun(A) of
      true -> {continue,
        #machine_state{
          pc = #pc{
            pc = B,
            instruction = memget(B, Memory)
          }
        },
        VmState
      };
      _ -> {continue, #machine_state{}, VmState}
    end
  end}.

%% @doc Writes the given value to the given output.
-spec write_output(output(), value()) -> output().
write_output(Output, Value) ->
  intcode_io:push(Output, Value).

%% @doc Attempts to read input from the VM state, and if that fails, requests
%% a notification from the input once input becomes available.
-spec read_input(CurrentMachineState :: machine_state(), CurrentVmState :: vm_state()) ->
  {sleep, NewVmState :: vm_state()} | {ok, Result :: value(), NewVmState :: vm_state()}.
read_input(MachineState, #vm_state{input = Input, input_callback = InputCallback} = VmState) ->
  case intcode_io:poll_or_notify(Input, fun() -> InputCallback(MachineState, VmState) end) of
    {V, NewInput} when V == nil orelse V == wait -> {sleep, VmState#vm_state{input = NewInput}};
    {V, NewInput} -> {ok, V, VmState#vm_state{input = NewInput}}
  end.
