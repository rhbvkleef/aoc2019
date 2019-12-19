-module(intcode_io).
-author("rolf").

-callback poll(string() | {string(), node()} | {global, string()} | {via, module(), string()} | pid()) -> integer().
-callback push(string() | {string(), node()} | {global, string()} | {via, module(), string()} | pid(), integer()) -> ok.
