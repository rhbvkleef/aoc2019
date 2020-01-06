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

get_module(Reference) ->
  element(1, Reference).

%% @doc Pushes `Value' onto the queue described by `Descriptor'.
%%
%% Calls the `push/2' function on the module that is defined by the first
%% element of the tuple of the first parameter with the same parameters that
%% are passed to this function.
-spec push(Reference, Value :: intcode:value()) -> Reference when Reference :: intcode_io().
push(Reference, Value) ->
  Module = get_module(Reference),
  Module:push(Reference, Value).

%% @doc Polls a value from the queue described by `Descriptor'.
%%
%% Calls the `poll/1' function on the module that is defined by the first
%% element of the tuple of the first parameter with the same parameter that
%% is passed to this function.
-spec poll(Reference) -> nil | {intcode:value(), Reference} when Reference :: intcode_io().
poll(Reference) ->
  Module = get_module(Reference),
  Module:poll(Reference).

%% @doc Attempts to poll a value from the queue described by `Descriptor', but
%% if that fails, calls the `Callback' function once a value becomes available.
%%
%% Calls the `async_poll/2' function on the module that is defined by the first
%% element of the tuple of the first parameter with the same parameters that
%% are passed to this function.
-spec poll_or_notify(Reference, Callback :: fun(() -> any())) -> {intcode:value() | wait, Reference} when Reference :: intcode_io().
poll_or_notify(Reference, Callback) ->
  Module = get_module(Reference),
  Module:poll_or_notify(Reference, Callback).

%% @doc Converts the queue described by `Descriptor' into a list.
%%
%% Calls the `as_list/1' function on the module that is defined by the first
%% element of the tuple of the first parameter with the same parameter that
%% is passed to this function.
-spec as_list(Reference :: intcode_io()) -> list(intcode:value()).
as_list(Reference) ->
  Module = get_module(Reference),
  Module:as_list(Reference).
