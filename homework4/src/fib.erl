-module(fib).
-export([fibnum/1]).

fibnum(N) ->
    fibnum(N,1,1).

fibnum(2,A,B) -> 
    A+B;
fibnum(N,A,B) ->
    fibnum(N-1, B,A+B).





