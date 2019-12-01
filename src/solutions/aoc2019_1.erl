-module(aoc2019_1).

-export([
    a/1,
    b/1
]).

sumof(Fn, List) -> lists:foldl(fun(X, Sum) -> Fn(X) + Sum end, 0, List).

fuel_for(Mass) -> floor(Mass / 3) - 2.
fuel_for_rocket(Mass) ->
    N = fuel_for(Mass),
    if 
        N =< 0 -> 0;
        true -> N + fuel_for_rocket(N)
    end.

a(Lines) -> sumof(
    fun(L) -> fuel_for(list_to_integer(L)) end,
    Lines
).

b(Lines) -> sumof(
    fun(L) -> fuel_for_rocket(list_to_integer(L)) end,
    Lines
).
