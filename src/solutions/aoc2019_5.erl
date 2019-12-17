-module(aoc2019_5).

-export([
    a/1,
    b/1
]).

-import(intcode, [out/1, run/2, read_program/1]).

a([Line|_]) ->
    Program = read_program(Line),
    Input = [1],
    out(run(Program, Input)).

b([Line|_]) ->
    Program = read_program(Line),
    Input = [5],
    out(run(Program, Input)).
