-module(aoc2019_8_tests).

-include_lib("eunit/include/eunit.hrl").

-import(aoc2019_8, [do_a/3, do_b/3]).

a_test() -> ?assertEqual(do_a(3, 2, "123456789012"), 1).
b_test() -> ?assertEqual(do_b(2, 2, "0222112222120000"), "\n01\n10").
