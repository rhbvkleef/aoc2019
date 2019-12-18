-module(aoc2019_6).

-export([
    a/1,
    b/1
]).

-include_lib("eunit/include/eunit.hrl").

-spec structure(list({string(), string()})) -> #{string() => list(string())}.
structure([]) -> maps:new();
structure([{Object,Sattelite}|Orbits]) ->
    Rest = structure(Orbits),
    maps:put(Object, [Sattelite|maps:get(Object, Rest, [])], Rest).

-spec fst({T, _}) -> T.
fst({A, _}) -> A.

-spec snd({_, T}) -> T.
snd({_, B}) -> B.

-spec count(T, #{T => list(string())}) -> {integer(), integer()}.
count(Root, Map) ->
    NewRoots = maps:get(Root, Map, []),
    Children = [count(NewRoot, Map) || NewRoot <- NewRoots],
    {length(Children) + lists:sum(lists:map(fun fst/1, Children)), length(Children) + lists:sum(lists:map(fun fst/1, Children)) + lists:sum(lists:map(fun snd/1, Children))}.

-spec parse(string()) -> {string(), string()}.
parse(Line) -> [Object,Sattelite] = string:tokens(Line, ")"), {Object, Sattelite}.

-spec path_to(string(), string(), #{string() => list(string())}) -> false | list(string()).
path_to(Target, Target, _) -> [];
path_to(Target, Root, Map) ->
    Children = maps:get(Root, Map, []),
    case lists:search(fun (false) -> false; (_) -> true end, lists:map(fun (Child) -> path_to(Target, Child, Map) end, Children)) of
        {value, V} -> [Root|V];
        false -> false
    end.

accumulate([]) -> maps:new();
accumulate([X|Xs]) ->
    Duplicates = accumulate(Xs),
    maps:put(X, maps:get(X, Duplicates, 0) + 1, Duplicates).

-spec a(list(string())) -> integer().
a(Lines) -> snd(count("COM", structure(lists:map(fun parse/1, Lines)))).

-spec b(list(string())) -> integer().
b(Lines) ->
    Structure = structure(lists:map(fun parse/1, Lines)),
    length(lists:filter(fun (X) -> X == 1 end, maps:values(accumulate(
        path_to("SAN", "COM", Structure) ++
        path_to("YOU", "COM", Structure))))).
