-module(aoc2019_9_tests).

-include_lib("eunit/include/eunit.hrl").

-import(aoc2019_9, [a/1, b/1]).
-import(intcode_server, [output/1, run_sync/3]).

intcode_test() -> ?assertEqual(output(run_sync([109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99], [], [])), [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]),
  ?assertEqual(output(run_sync([1102,34915192,34915192,7,4,7,99,0], [], [])), [1219070632396864]),
  ?assertEqual(output(run_sync([104,1125899906842624,99], [], [])), [1125899906842624]).
