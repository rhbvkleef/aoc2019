-module(aoc2019_test).

-include_lib("eunit/include/eunit.hrl").

foo_test() ->
    ?assertEqual(-100, -(100)),
    ?assertEqual(100, -(-100)).
