-type continuation_method() :: continue | sleep | halt.
%% An instruction method can use these values to instruct the VM on how to
%% proceed. `sleep' is commonly used when new input is desired, `continue'
%% is used when the machine should just keep on executing. `halt' shuts down
%% the VM.

-type instruction_arity() :: integer().
-type value() :: integer().
-type address() :: value().
-type memory() :: array:array(value()).
-type input() :: intcode_io:intcode_io().
-type output() :: intcode_io:intcode_io().
-type instruction_argument() :: {address(), value()}.

-record(pc, {
  pc :: address(),
  instruction :: value()
}).
-type pc() :: #pc{pc :: address(), instruction :: value()}.

-record(machine_state, {
  pc = nil :: pc() | nil,
  mem = nil :: memory() | nil,
  output = nil :: output() | nil,
  relbase = nil :: value() | nil
}).
-type machine_state() :: #machine_state{pc :: pc(), mem :: memory(), output :: output(), relbase :: value()}.
-type partial_machine_state() :: machine_state() | #machine_state{pc :: pc() | nil, mem :: memory() | nil, output :: output() | nil, relbase :: value() | nil}.

-record(vm_state, {
  name = nil :: string() | nil,
  input = [] :: list(value) | {module(), reference() | pid()} | queue:queue(value()),
  input_callback :: fun((machine_state(), vm_state()) -> any()),
  shutdown_listeners = [] :: list({pid(), term()})
}).
-type vm_state() :: #vm_state{
  name :: string() | nil,
  input :: list(value) | {module(), reference() | pid()} | queue:queue(value()),
  input_callback :: fun((machine_state(), vm_state()) -> any()),
  shutdown_listeners :: list({pid(), term()})
}.

-type instruction() :: {instruction_arity(), fun((list(instruction_argument()), machine_state(), vm_state()) -> {continuation_method(), machine_state(), vm_state()})}.
