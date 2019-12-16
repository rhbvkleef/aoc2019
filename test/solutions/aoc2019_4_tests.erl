-module(aoc2019_4_tests).

-include_lib("eunit/include/eunit.hrl").

count_test() -> ?assertEqual(aoc2019_4:count($a, "ab"), 1),
    ?assertEqual(aoc2019_4:count($a, "aab"), 2),
    ?assertEqual(aoc2019_4:count($a, "aaab"), 3).
