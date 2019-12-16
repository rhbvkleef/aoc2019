-module(aoc2019_3).

-export([
    a/1,
    b/1
]).

minimum([]) -> io:format("can not find minimum of empty list~n");
minimum([H|T]) -> minimum(H, T).

minimum(Min, [H|T]) when Min < H -> minimum(Min, T);
minimum(_, [H|T]) -> minimum(H, T);
minimum(Min, []) -> Min.

positions(X, Y, L, [$U|Num]) -> {[{X, Y+NewY, L+NewY} || NewY <- lists:seq(1, list_to_integer(Num))], X, Y+list_to_integer(Num), L+list_to_integer(Num)};
positions(X, Y, L, [$D|Num]) -> {[{X, Y-NewY, L+NewY} || NewY <- lists:seq(1, list_to_integer(Num))], X, Y-list_to_integer(Num), L+list_to_integer(Num)};
positions(X, Y, L, [$L|Num]) -> {[{X+NewX, Y, L+NewX} || NewX <- lists:seq(1, list_to_integer(Num))], X+list_to_integer(Num), Y, L+list_to_integer(Num)};
positions(X, Y, L, [$R|Num]) -> {[{X-NewX, Y, L+NewX} || NewX <- lists:seq(1, list_to_integer(Num))], X-list_to_integer(Num), Y, L+list_to_integer(Num)}.

positions([], Poss, _, _, _) -> Poss;
positions([O|Os], Poss, X, Y, L) ->
    {NewPoss, NewX, NewY, NewL} = positions(X, Y, L, O),
    positions(Os, lists:foldl(fun ({TX, TY, TL}, TA) -> maps:put({TX, TY}, TL, TA) end, Poss, NewPoss), NewX, NewY, NewL).

answer_a(M1, M2) -> minimum([abs(X)+abs(Y) || {{X, Y}, _} <- maps:to_list(M1), maps:is_key({X, Y}, M2)]).
answer_b(M1, M2) -> minimum([S1+maps:get(K, M2) || {K, S1} <- maps:to_list(M1), maps:is_key(K, M2)]).
% intersection(L1, L2) -> [abs(X1)+abs(X2) || {X1, X2} <- L1, {Y1, Y2} <- L2, X1 =:= Y1, X2 =:= Y2].

a([L1,L2|_]) -> answer_a(positions(string:tokens(L1, ","), maps:new(), 0, 0, 0), positions(string:tokens(L2, ","), maps:new(), 0, 0, 0)).
b([L1,L2|_]) -> answer_b(positions(string:tokens(L1, ","), maps:new(), 0, 0, 0), positions(string:tokens(L2, ","), maps:new(), 0, 0, 0)).
