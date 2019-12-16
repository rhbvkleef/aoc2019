-module(aoc2019).

-export([
	 start/2,
	 find_modules/0
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
	io:format("A: ~w\n", [Module:a(Lines)]),
	io:format("B: ~w\n", [Module:b(Lines)]).

start(_Mode, _Args) ->
	lists:map(fun run/1, find_modules()),
	{ok, self()}.
