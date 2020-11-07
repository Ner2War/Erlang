-module(p05).
-export([reverse/1]).
reverse(L) ->
    reverse(L, []).

reverse([H|T], Acc) ->
    reverse(T, [H|Acc]);
reverse([], Acc) ->
    Acc.