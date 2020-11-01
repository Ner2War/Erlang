-module(p14).
-export([duplicate/1]).
duplicate(L) ->
    duplicate(L,[]).

duplicate([H|T],Acc) ->
    duplicate(T,[H,H|Acc]);
duplicate([],Acc) ->
    reverse(Acc,[]).

reverse([H|T], Acc) ->
    reverse(T, [H|Acc]);
reverse([], Acc) ->
    Acc.