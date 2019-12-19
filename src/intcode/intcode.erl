-module(intcode).

-define(NODEBUG, yes_indeed_dont_show_me_debug_messages_as_i_hate_them).
-include_lib("eunit/include/eunit.hrl").

-export([
    % Run functions
    run/1, run/2, run/3,
    % Utils
    read_program/1, mem/1, out/1, 'finished?'/1,
    % GenServer
    init/1, handle_call/3, handle_cast/2, start_link/4
]).

-behaviour(gen_server).

-record(pc, {
    pc :: integer(),
    instruction :: integer()
}).

-type memory() :: array:array(integer()).
-type input(T) :: {module(), pid()} | list(T).
-type output(T) :: {module(), pid()} | list(T).
-type pc() :: #pc{pc::integer(), instruction::integer()}.

-spec execute(pc(), memory(), _, [any()]) -> {pc(), memory(), _, [any()]}.
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
execute(#pc{instruction=I} = Pc, Memory, Input, Output) when I rem 100 == 3 ->
    [{Aoff, _}] = read_instruction(Pc, Memory, 1),
    {X, NewInput} = read_input(Input),
    NewMem = array:set(Aoff, X, Memory),
    NewPc = increment_pc(Pc, NewMem, 1),
    {
        NewPc,
        NewMem,
        NewInput,
        Output
    };
% 04 (OUT): ?OUT = A; ?PC = ?PC + 2
execute(#pc{instruction=I} = Pc, Memory, Input, Output) when I rem 100 == 4 ->
    [{_, A}] = read_instruction(Pc, Memory, 1),
    NewPc = increment_pc(Pc, Memory, 1),
    NewOutput = write_output(Output, A),
    {
        NewPc,
        Memory,
        Input,
        NewOutput
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
-spec read_instruction(pc(), memory(), non_neg_integer()) -> list({integer(), integer()}).
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

-spec increment_pc(pc(), memory(), non_neg_integer()) -> pc().
increment_pc(#pc{pc=I}, Memory, Arity) ->
    #pc{pc=I+Arity+1, instruction=array:get(I+Arity+1, Memory)}.

procname() ->
    case process_info(self(), registered_name) of
        {registered_name, Name} -> Name;
        _ -> self()
    end.

-spec read_input(input(T)) -> {T, input(T)}.
read_input({Module, Pid}) ->
    Input = Module:poll(Pid),
    ?debugFmt("~w: read ~w", [procname(), Input]),
    {Input, {Module, Pid}};
read_input([V|Vs]) -> {V, Vs}.

-spec write_output(output(T), T) -> output(T).
write_output({Module, Pid}, Value) ->
    ?debugFmt("~w: wrote ~w", [procname(), Value]),
    Module:push(Pid, Value), {Module, Pid};
write_output(Vs, Value) when is_list(Vs) -> [Value|Vs].

-spec run(pc(), memory(), input(integer()), output(integer())) -> {memory(), output(integer)}.
run(#pc{pc = -1}, Memory, _, Output) ->
    {Memory, Output};
run(#pc{} = Pc, Memory, Input, Output) ->
    {NewPc, NewMemory, NewInput, NewOutput} = execute(Pc, Memory, Input, Output),
    ?debugFmt("~w: New state: ~w", [procname(), NewPc]),
    run(NewPc, NewMemory, NewInput, NewOutput).

-spec run(memory()) -> {list(integer()), output(integer())}.
run(Memory) -> run(Memory, []).

-spec run(memory(), input(integer())) -> {list(integer()), output(integer())}.
run(Memory, Input) -> run(Memory, Input, []).

-spec run(memory(), input(integer()), output(integer())) -> {list(integer()), output(integer())}.
run(Memory, Input, Output) ->
    Mem = array:from_list(Memory),
    Pc = #pc{pc=0, instruction=array:get(0, Mem)},
    ?debugFmt("~w: New state: ~w", [procname(), Pc]),
    {NewMem, NewOutput} = run(Pc, Mem, Input, Output),
    {array:to_list(NewMem), NewOutput}.

-spec read_program(string()) -> list(integer()).
read_program(Line) ->
    [list_to_integer(I) || I <- string:tokens(Line, ",")].

-spec mem({memory(), _} | pid()) -> memory().
mem(Pid) when is_pid(Pid) -> gen_server:call(Pid, mem);
mem({Mem, _Out}) -> Mem.

-spec out({_, output(integer())} | pid()) -> output(integer()).
out(Pid) when is_pid(Pid) -> gen_server:call(Pid, out);
out({_Mem, Out}) when is_list(Out) -> lists:reverse(Out);
out({_Mem, Out}) -> Out.

'finished?'(Pid) -> gen_server:call(Pid, 'finished?').

init({Args, Name}) ->
    case Name of
        nil -> nil;
        Name -> register(Name, self())
    end,
    gen_server:cast(self(), run),
    {ok, Args}.

start_link(Memory, Input, Output, Name) -> gen_server:start_link(intcode, {{Memory, Input, Output}, Name}, []).

handle_call(out, _From, State) -> {reply, out(State), State};
handle_call(mem, _From, State) -> {reply, mem(State), State};
handle_call('finished?', _From, State) -> {reply, ok, State}.

handle_cast(run, {Memory, Input, Output}) -> {noreply, run(Memory, Input, Output)}.
