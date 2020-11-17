-module(test).
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

fib_test_() -> [
    ?_assert(fib:fibnum(5) =:= 8),
    ?_assert(fib:fibnum(10) =:= 89),
    ?_assert(fib:fibnum(15) =:= 987)
].
-endif.

