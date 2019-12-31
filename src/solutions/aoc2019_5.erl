-module(aoc2019_5).

-export([
    a/1,
    b/1
]).

-import(intcode_server, [run_sync/3, output/1]).
-import(intcode, [read_program/1]).

a([Line|_]) ->
    lists:last(output(run_sync(read_program(Line), [1], []))).

b([Line|_]) ->
    lists:last(output(run_sync(read_program(Line), [5], []))).
