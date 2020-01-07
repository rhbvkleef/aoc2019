-module(aoc2019_9).

-export([
    a/1,
    b/1
]).

a([Line|_]) -> lists:nth(1, intcode_server:output(intcode_server:run_sync(intcode:read_program(Line), [1], []))).
b([Line|_]) -> lists:nth(1, intcode_server:output(intcode_server:run_sync(intcode:read_program(Line), [2], []))).
