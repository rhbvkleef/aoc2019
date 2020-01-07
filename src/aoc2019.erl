-module(aoc2019).

-export([
	 start/2
]).

load(I, Ms) ->
	case code:ensure_loaded(list_to_atom("aoc2019_" ++ integer_to_list(I))) of
		{module, Module} -> load(I + 1, [Module | Ms]);
		{error, _} -> Ms
	end.

find_modules() -> load(1, []).

readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try get_all_lines(Device)
      after file:close(Device)
    end.

get_all_lines(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line -> [string:trim(Line)|get_all_lines(Device)]
    end.

run(Module) ->
	Lines = readlines("inputs/" ++ atom_to_list(Module) ++ ".txt"),
	io:format("Module: ~s\n", [Module]),
	A = Module:a(Lines),
	case is_list(A) of
		true -> io:format("A: ~s\n", [A]);
		_ -> io:format("A: ~w\n", [A])
	end,
	B = Module:b(Lines),
	case is_list(B) of
		true -> io:format("B: ~s\n", [B]);
		_ -> io:format("B: ~w\n", [B])
	end.

%% @doc Run the `aoc2019' application.
%% This application runs all solutions for the 2019 session of
%% <a href="https://adventofcode.com">Advent Of Code</a>.
start(_Mode, _Args) ->
	lists:map(fun run/1, find_modules()),
	{ok, self()}.
