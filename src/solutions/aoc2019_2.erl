-module(aoc2019_2).

-import(intcode, [read_program/1]).
-import(intcode_server, [run_sync/3, memory/1]).

-export([
  a/1,
  b/1
]).

a([Line | _]) ->
  [F, _, _|Rest] = read_program(Line),
  Program = [F, 12, 2|Rest],
  [Return|_] = memory(run_sync(Program, [], [])),
  Return.

b([Line | _]) ->
  [F, _, _|Rest] = read_program(Line),
  [{RA, RB}|_] = [{A, B} || A <- lists:seq(0, 99), B <- lists:seq(0, 99), begin [Return|_] = memory(run_sync([F, A, B | Rest], [], [])), Return == 19690720 end],
  (100 * RA) + RB.
