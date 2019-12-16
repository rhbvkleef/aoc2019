-module(aoc2019_2).

-export([
    a/1,
    b/1
]).

a([Line|_]) ->
    [F,_,_|Rest] = intcode:read_program(Line),
    Program = [F,12,2|Rest],
    [Return|_] = intcode:run(Program),
    Return.

b([Line|_]) ->
    [F,_,_|Rest] = intcode:read_program(Line),
    [{RA, RB}|_] = [{A, B} || A <- lists:seq(0, 99), B <- lists:seq(0, 99), begin
        [Return|_] = intcode:run([F,A,B|Rest]),
        Return == 19690720
    end],
    (100 * RA) + RB.
