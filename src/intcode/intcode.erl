%% @doc
%% This module defines `intcode' instruction set along with providing tools to read `intcode' programs from a string.
-module(intcode).

-include("intcode.hrl").

-export([
  read_program/1,
  instruction/1
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
%% Defines the `intcode' instruction set:
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
%%       <td>Set `#C=1' when `A&lt;B', otherwise set `#C=0'</td>
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
instruction(99) -> {0, fun(_, #machine_state{pc = Pc}, VmState) -> {halt, #machine_state{pc = Pc}, VmState} end}.

-spec memset(address(), value(), memory()) -> memory().
memset(Addr, Value, Mem) -> array:set(Addr, Value, Mem).

-spec memget(address(), memory()) -> value().
memget(Addr, Mem) -> array:get(Addr, Mem).

-spec alu(fun((value(), value()) -> value())) -> instruction().
alu(Fun) ->
  {3,
    fun([{_, A}, {_, B}, {Coff, _}], #machine_state{mem = Memory}, VmState) ->
      {continue, #machine_state{
        mem = memset(Coff, Fun(A, B), Memory)
      }, VmState}
    end
  }.

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

-spec write_output(output(), value()) -> output().
write_output(Output, Value) ->
  intcode_io:push(Output, Value).

-spec read_input(CurrentMachineState :: machine_state(), CurrentVmState :: vm_state()) ->
  {sleep, NewVmState :: vm_state()} | {ok, Result :: value(), NewVmState :: vm_state()}.
read_input(MachineState, #vm_state{input = Input, input_callback = InputCallback} = VmState) ->
  case intcode_io:poll_or_notify(Input, fun() -> InputCallback(MachineState, VmState) end) of
    {V, NewInput} when V == nil orelse V == wait -> {sleep, VmState#vm_state{input = NewInput}};
    {V, NewInput} -> {ok, V, VmState#vm_state{input = NewInput}}
  end.

