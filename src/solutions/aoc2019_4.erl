-module(aoc2019_4).

-export([
    a/1,
    b/1,
    count/2
]).

-include_lib("eunit/include/eunit.hrl").

increasing([]) -> true;
increasing([_]) -> true;
increasing([X,Y|_]) when X > Y -> false;
increasing([_|R]) -> increasing(R).

n_thesame([]) -> 0;
n_thesame([_]) -> 0;
n_thesame([X,X|Rest]) -> 1 + n_thesame([X|Rest]);
n_thesame([_,Y|Rest]) -> n_thesame([Y|Rest]).

count(_, []) -> 0;
count(X, [X|Xs]) -> 1 + count(X, Xs);
count(_, _) -> 0.

n_thesame_b([]) -> 0;
n_thesame_b([_]) -> 0;
n_thesame_b([X,Y|Rest]) -> case count(X, [Y|Rest]) of
    1 -> 1 + n_thesame_b(Rest);
    0 -> n_thesame_b([Y|Rest]);
    Count -> n_thesame_b(lists:sublist(Rest, Count, length(Rest)))
end.

valid_a(Password) ->
    PwStr = integer_to_list(Password),
    increasing(PwStr) andalso n_thesame(PwStr) >= 1.

valid_b(Password) ->
    PwStr = integer_to_list(Password),
    increasing(PwStr) andalso n_thesame_b(PwStr) >= 1.

a([Input|_]) ->
    [L,H] = [list_to_integer(X) || X <- string:tokens(Input, "-")],
    length([Password || Password <- lists:seq(L, H), valid_a(Password)]).
b([Input|_]) ->
    [L,H] = [list_to_integer(X) || X <- string:tokens(Input, "-")],
    length([Password || Password <- lists:seq(L, H), valid_b(Password)]).
