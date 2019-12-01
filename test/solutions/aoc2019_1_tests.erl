-module(aoc2019_1_tests).

-include_lib("eunit/include/eunit.hrl").

a_test() ->
    ?assertEqual(aoc2019_1:a(["12"]), 2),
    ?assertEqual(aoc2019_1:a(["14"]), 2),
    ?assertEqual(aoc2019_1:a(["1969"]), 654),
    ?assertEqual(aoc2019_1:a(["100756"]), 33583).

b_test() ->
    ?assertEqual(aoc2019_1:b(["12"]), 2),
    ?assertEqual(aoc2019_1:b(["1969"]), 966),
    ?assertEqual(aoc2019_1:b(["100756"]), 50346).
