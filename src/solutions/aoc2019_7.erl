-module(aoc2019_7).

-export([
    a/1,
    b/1
]).

-import(intcode, [out/1, run/2, read_program/1, start_link/4, 'finished?'/1]).

perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

a([Line|_]) ->
    Program = read_program(Line),
    lists:max([lists:foldl(fun (P, A) -> [R|_] = out(run(Program, [P, A])), R end, 0, X) || X <- perms([0, 1, 2, 3, 4])]).

b([Line|_]) ->
    Program = read_program(Line),
    lists:max([attempt(Program, X) || X <- perms([5, 6, 7, 8, 9])]).

attempt(Program, [Pa, Pb, Pc, Pd, Pe]) ->
    {ok, AbBuf} = async_queue:start_link(nil),
    async_queue:push(AbBuf, Pa),
    {ok, BcBuf} = async_queue:start_link(nil),
    async_queue:push(BcBuf, Pb),
    {ok, CdBuf} = async_queue:start_link(nil),
    async_queue:push(CdBuf, Pc),
    {ok, DeBuf} = async_queue:start_link(nil),
    async_queue:push(DeBuf, Pd),
    {ok, EaBuf} = async_queue:start_link(nil),
    async_queue:push(EaBuf, Pe),
    async_queue:push(EaBuf, 0),
    {ok, _} = start_link(Program, {async_queue, BcBuf}, {async_queue, CdBuf}, nil),
    {ok, _} = start_link(Program, {async_queue, CdBuf}, {async_queue, DeBuf}, nil),
    {ok, _} = start_link(Program, {async_queue, AbBuf}, {async_queue, BcBuf}, nil),
    {ok, _} = start_link(Program, {async_queue, EaBuf}, {async_queue, AbBuf}, nil),
    {ok, E} = start_link(Program, {async_queue, DeBuf}, {async_queue, EaBuf}, nil),
    'finished?'(E),
    async_queue:poll(EaBuf).
