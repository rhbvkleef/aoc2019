-module(aoc2019_8).

-export([
    a/1,
    b/1,
    do_a/3,
    do_b/3
]).

layers(Size, ImgData) when length(ImgData) < Size -> [];
layers(Size, ImgData) ->
    {Layer, Rest} = lists:split(Size, ImgData),
    [Layer | layers(Size, Rest)].

count(_, []) -> 0;
count(V, [V|Rest]) -> count(V, Rest) + 1;
count(V, [_|Rest]) -> count(V, Rest).

do_a(Width, Height, Line) ->
    Layer = element(2, lists:nth(1, lists:keysort(1, [{count($0, L), L} || L <- layers(Width * Height, Line)]))),
    count($1, Layer) * count($2, Layer).

fill_transparency([], _) -> [];
fill_transparency(_, []) -> [];
fill_transparency([$2|CurrentLayer], [X|NextLayer]) -> [X|fill_transparency(CurrentLayer, NextLayer)];
fill_transparency([X|CurrentLayer], [_|NextLayer]) -> [X|fill_transparency(CurrentLayer, NextLayer)].

do_b(Width, Height, Line) ->
    [FstLayer|Layers] = lists:reverse(layers(Width * Height, Line)),
    [$\n | string:join(layers(Width, lists:foldl(fun fill_transparency/2, FstLayer, Layers)), "\n")].

a([Line|_]) -> do_a(25, 6, Line).
b([Line|_]) -> do_b(25, 6, Line).
