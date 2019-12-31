-module(aoc2019_7).

-export([
  a/1,
  b/1
]).

-import(intcode, [read_program/1]).
-import(intcode_server, [run_sync/3, output/1]).

perms([]) -> [[]];
perms(L) -> [[H | T] || H <- L, T <- perms(L--[H])].

a([Line | _]) ->
  Program = read_program(Line),
  lists:max([lists:foldl(fun(P, A) -> [R | _] = output(run_sync(Program, [P, A], [])), R end, 0, X) || X <- perms([0, 1, 2, 3, 4])]).

b([Line | _]) ->
  Program = read_program(Line),
  lists:max([attempt(Program, X) || X <- perms([5, 6, 7, 8, 9])]).

attempt(Program, [Pa, Pb, Pc, Pd, Pe]) ->
  EaBuf = intcode_io_async_queue:new([Pa, 0]),
  E = intcode_server:new(Program, [Pe], EaBuf),
  D = intcode_server:new(Program, [Pd], E),
  C = intcode_server:new(Program, [Pc], D),
  B = intcode_server:new(Program, [Pb], C),
  A = intcode_server:new(Program, EaBuf, B),
  lists:foreach(fun intcode_server:start/1, [A, B, C, D, E]),
  intcode_server:'finished?'(E),
  {Result, _} = intcode_io:poll(EaBuf),
  lists:foreach(fun intcode_server:stop/1, [A, B, C, D, E]),
  intcode_io_async_queue:stop(EaBuf),
  Result.
