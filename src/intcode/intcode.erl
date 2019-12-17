-module(intcode).
-export([
    run/2,
    read_program/1,
    mem/1,
    out/1
]).

-include_lib("eunit/include/eunit.hrl").

-type memory() :: array:array(integer()).

-record(pc, {
    pc :: integer(),
    instruction:: integer()
}).

-spec execute(#pc{}, memory(), list(integer()), list(integer())) -> {#pc{}, memory(), list(integer()), list(integer())}.
% 01 (ADD): #C = A + B; ?PC = ?PC + 4
execute(#pc{instruction=I} = Pc, Memory, Input, Output) when I rem 100 == 1 ->
    [{_, A}, {_, B}, {Coff, _}] = read_instruction(Pc, Memory, 3),
    NewMem = array:set(Coff, A + B, Memory),
    NewPc = increment_pc(Pc, NewMem, 3),
    {
        NewPc,
        NewMem,
        Input,
        Output
    };
% 02 (MUL): #C = A * B; ?PC = ?PC + 4
execute(#pc{instruction=I} = Pc, Memory, Input, Output) when I rem 100 == 2 ->
    [{_, A}, {_, B}, {Coff, _}] = read_instruction(Pc, Memory, 3),
    NewMem = array:set(Coff, A * B, Memory),
    NewPc = increment_pc(Pc, NewMem, 3),
    {
        NewPc,
        NewMem,
        Input,
        Output
    };
% 03 (INP): #A = ?IN; ?PC = ?PC + 2
execute(#pc{instruction=I} = Pc, Memory, [X|Input], Output) when I rem 100 == 3 ->
    [{Aoff, _}] = read_instruction(Pc, Memory, 1),
    NewMem = array:set(Aoff, X, Memory),
    NewPc = increment_pc(Pc, NewMem, 1),
    {
        NewPc,
        NewMem,
        Input,
        Output
    };
% 04 (OUT): ?OUT = A; ?PC = ?PC + 2
execute(#pc{instruction=I} = Pc, Memory, Input, Output) when I rem 100 == 4 ->
    [{_, A}] = read_instruction(Pc, Memory, 1),
    NewPc = increment_pc(Pc, Memory, 1),
    {
        NewPc,
        Memory,
        Input,
        [A|Output]
    };
% 05 (IFV): ?PC = B if A =/= 0 else (?PC + 3)
execute(#pc{instruction=I} = Pc, Memory, Input, Output) when I rem 100 == 5 ->
    [{_, A}, {_, B}] = read_instruction(Pc, Memory, 2),
    case A of
        0 -> NewPc = increment_pc(Pc, Memory, 2),
            {
                NewPc,
                Memory,
                Input,
                Output
            };
        _ -> NewPc = #pc{pc=B, instruction=array:get(B, Memory)},
            {
                NewPc,
                Memory,
                Input,
                Output
            }
    end;
% 06 (IFZ): ?PC = B if A == 0 else (?PC + 3)
execute(#pc{instruction=I} = Pc, Memory, Input, Output) when I rem 100 == 6 ->
    [{_, A}, {_, B}] = read_instruction(Pc, Memory, 2),
    case A of
        0 -> NewPc = #pc{pc=B, instruction=array:get(B, Memory)},
            {
                NewPc,
                Memory,
                Input,
                Output
            };
        _ -> NewPc = increment_pc(Pc, Memory, 2),
            {
                NewPc,
                Memory,
                Input,
                Output
            }
    end;
% 07 (CLT): #C = (1 if A < B else 0); ?PC = ?PC + 4
execute(#pc{instruction=I} = Pc, Memory, Input, Output) when I rem 100 == 7 ->
    [{_, A}, {_, B}, {Coff, _}] = read_instruction(Pc, Memory, 3),
    NewMem = array:set(Coff, case A < B of true -> 1; _ -> 0 end, Memory),
    NewPc = increment_pc(Pc, Memory, 3),
    {
        NewPc,
        NewMem,
        Input,
        Output
    };
% 08 (CEQ): #C = (1 if A == B else 0); ?PC = ?PC + 4
execute(#pc{instruction=I} = Pc, Memory, Input, Output) when I rem 100 == 8 ->
    [{_, A}, {_, B}, {Coff, _}] = read_instruction(Pc, Memory, 3),
    NewMem = array:set(Coff, case A == B of true -> 1; _ -> 0 end, Memory),
    NewPc = increment_pc(Pc, Memory, 3),
    {
        NewPc,
        NewMem,
        Input,
        Output
    };
% 99 (HLT): ?IW = -1, ?PC = -1 (effectively halts the machine)
execute(#pc{instruction=I}, Memory, Input, Output) when I rem 100 == 99 ->
    {
        #pc{instruction = -1, pc = -1},
        Memory,
        Input,
        Output
    };
execute(#pc{} = Pc, Memory, _, _) ->
    erlang:error({x_bad_instruction, Pc, array:to_list(Memory)}).

% For some reason, the length of the Modes array will be one longer (too long?) than lists:seq(1, Arity),
% and I am not sure, so I just pad it to one less.
-spec read_instruction(#pc{}, memory(), non_neg_integer()) -> list(integer()).
read_instruction(#pc{pc=Pos, instruction=I}, Memory, Arity) ->
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
    Vs.

increment_pc(#pc{pc=I}, Memory, Arity) ->
    #pc{pc=I+Arity+1, instruction=array:get(I+Arity+1, Memory)}.

-spec run(#pc{}, memory(), list(integer()), list(integer())) -> {memory(), list(integer())}.
run(#pc{pc = -1}, Memory, _, Output) ->
    {Memory, Output};
run(#pc{} = Pc, Memory, Input, Output) ->
    {NewPc, NewMemory, NewInput, NewOutput} = execute(Pc, Memory, Input, Output),
    run(NewPc, NewMemory, NewInput, NewOutput).

-spec run(list(integer()), list(integer())) -> {list(integer()), list(integer())}.
run(Memory, Input) ->
    Mem = array:from_list(Memory),
    {NewMem, Output} = run(#pc{pc=0, instruction=array:get(0, Mem)}, Mem, Input, []),
    {array:to_list(NewMem), lists:reverse(Output)}.

read_program(Line) ->
    [list_to_integer(I) || I <- string:tokens(Line, ",")].

mem({Mem, _}) -> Mem.
out({_, Out}) -> Out.
