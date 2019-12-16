-module(intcode).
-export([
    run/1,
    read_program/1
]).

-include_lib("eunit/include/eunit.hrl").

-type memory() :: array:array(integer()).

-record(pc, {
    pc :: integer(),
    instruction:: integer()
}).

-spec execute(#pc{}, memory()) -> {#pc{}, memory()}.
execute(#pc{instruction=1} = Pc, Memory) ->
    [Aoff, Boff, Coff] = read_instruction(Pc, Memory, 3),
    NewMem = array:set(Coff,
            array:get(Aoff, Memory) + array:get(Boff, Memory),
            Memory),
    NewPc = increment_pc(Pc, NewMem, 3),
    {
        NewPc,
        NewMem
    };
execute(#pc{instruction=2} = Pc, Memory) ->
    [Aoff, Boff, Coff] = read_instruction(Pc, Memory, 3),
    NewMem = array:set(Coff,
            array:get(Aoff, Memory) * array:get(Boff, Memory),
            Memory),
    NewPc = increment_pc(Pc, NewMem, 3),
    {
        NewPc,
        NewMem
    };
execute(#pc{instruction=99}, Memory) ->
    {
        #pc{instruction = -1, pc = -1},
        Memory
    };
execute(#pc{} = Pc, Memory) ->
    erlang:error({x_bad_instruction, Pc, array:to_list(Memory)}).

-spec read_instruction(#pc{}, memory(), non_neg_integer()) -> list(integer()).
read_instruction(#pc{pc = I}, Memory, Arity) ->
    Vs = [array:get(O + I, Memory) || O <- lists:seq(1, Arity)],
    Vs.

increment_pc(#pc{pc=I}, Memory, Arity) ->
    #pc{pc=I+Arity+1, instruction=array:get(I+Arity+1, Memory)}.

-spec run(#pc{}, memory()) -> memory().
run(#pc{pc = -1}, Memory) ->
    Memory;
run(#pc{} = Pc, Memory) ->
    {NewPc, NewMemory} = execute(Pc, Memory),
    run(NewPc, NewMemory).

-spec run(list(integer())) -> list(integer()).
run(Memory) ->
    Mem = array:from_list(Memory),
    array:to_list(run(#pc{pc=0, instruction=array:get(0, Mem)}, Mem)).

read_program(Line) ->
    [list_to_integer(I) || I <- string:tokens(Line, ",")].
