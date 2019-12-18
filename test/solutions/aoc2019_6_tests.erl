-module(aoc2019_6_tests).

-include_lib("eunit/include/eunit.hrl").

-import(aoc2019_6, [a/1, b/1]).

a_test() -> ?assertEqual(a(["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L"]), 42).
b_test() -> ?assertEqual(b(["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L", "K)YOU", "I)SAN"]), 5).
