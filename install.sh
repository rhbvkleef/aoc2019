#!/bin/bash
# vim: ts=4:sts=4:sw=4:et
# A script to download the input from AdventOfCode and to create the initial
# boilerplate.

YEAR=2019

source .credentials

if [ ! -f ./inputs/aoc${YEAR}_${1}.txt ]; then
    curl "https://adventofcode.com/2019/day/${1}/input" --cookie "session=${ADVENTOFCODE_SESSION}" > ./inputs/aoc${YEAR}_${1}.txt
    echo "Created ./inputs/aoc${YEAR}_${1}.txt"
else
    echo "./inputs/aoc${YEAR}_${1}.txt already exists."
fi

if [ ! -f ./src/solutions/aoc${YEAR}_${1}.erl ]; then
    echo "-module(aoc${YEAR}_$1).

-export([
    a/1,
    b/1
]).

a(Lines) -> false.
b(Lines) -> false." > ./src/solutions/aoc${YEAR}_${1}.erl
    echo "Created ./src/solutions/aoc${YEAR}_${1}.erl"
else
    echo "./src/solutions/aoc${YEAR}_${1}.erl already exists."
fi

if [ ! -f ./test/solutions/aoc${YEAR}_${1}_tests.erl ]; then
    echo "-module(aoc${YEAR}_${1}_tests).

-include_lib(\"eunit/include/eunit.hrl\").

-import(aoc${YEAR}_${1}, [a/1, b/1]).

a_test() -> ?assert(false).
b_test() -> ?assert(false)." > ./test/solutions/aoc${YEAR}_${1}_tests.erl
    echo "Created ./test/solutions/aoc${YEAR}_${1}_tests.erl"
else
    echo "./test/solutions/aoc${YEAR}_${1}_tests.erl already exists."
fi
