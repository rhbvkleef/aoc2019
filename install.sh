#!/bin/bash
# A script to download the input from AdventOfCode and to create the initial
# boilerplate.

YEAR=2019

source .credentials

curl "https://adventofcode.com/2019/day/${1}/input" --cookie "session=${ADVENTOFCODE_SESSION}" > ./inputs/aoc${YEAR}_${1}.txt

echo "-module(aoc${YEAR}_$1).

-export([
    a/1,
    b/1
]).

a(Lines) -> false.
b(Lines) -> false." > ./src/solutions/aoc${YEAR}_${1}.erl

echo "-module(aoc${YEAR}_${1}_tests).

-include_lib(\"eunit/include/eunit.hrl\").

a_test() -> ?assert(false).
b_test() -> ?assert(false)." > ./test/solutions/aoc${YEAR}_${1}_tests.erl
