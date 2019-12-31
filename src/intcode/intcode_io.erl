-module(intcode_io).

-export([
  push/2, poll/1, poll_or_notify/2, as_list/1
]).

-export_type([
  intcode_io/0
]).

-type intcode_io() :: tuple().

-callback poll(Reference) -> {intcode:value(), Reference}
  when Reference :: intcode_io().
-callback push(Reference, intcode:value()) -> Reference
  when Reference :: intcode_io().
-callback poll_or_notify(Reference, fun(() -> any())) -> {intcode:value(), Reference} | nil
  when Reference :: intcode_io().
-callback as_list(Reference) -> list(intcode:value())
  when Reference :: intcode_io().

get_module(Descriptor) ->
  element(1, Descriptor).

%% @doc
%% Calls the `push/2' function on the module that is defined by the first
%% element of the tuple of the first parameter with the same parameters that
%% are passed to this function.
push(Descriptor, Value) ->
  Module = get_module(Descriptor),
  Module:push(Descriptor, Value).

%% @doc
%% Calls the `poll/1' function on the module that is defined by the first
%% element of the tuple of the first parameter with the same parameter that
%% is passed to this function.
poll(Descriptor) ->
  Module = get_module(Descriptor),
  Module:poll(Descriptor).

%% @doc
%% Calls the `async_poll/2' function on the module that is defined by the first
%% element of the tuple of the first parameter with the same parameters that
%% are passed to this function.
poll_or_notify(Descriptor, Callback) ->
  Module = get_module(Descriptor),
  Module:poll_or_notify(Descriptor, Callback).

%% @doc
%% Calls the `as_list/1' function on the module that is defined by the first
%% element of the tuple of the first parameter with the same parameter that
%% is passed to this function.
as_list(Descriptor) ->
  Module = get_module(Descriptor),
  Module:as_list(Descriptor).
