-module(p07).
-export([flatten/1]).
flatten(L) -> 
       reverse(flatten(L, []), []).

flatten([H=[_|_]|T], Acc) ->
    flatten(T, flatten(H, Acc));
flatten([[]| T], Acc) ->
    flatten(T, Acc);
flatten([H|T], Acc) ->
     flatten(T, [H|Acc]);
flatten([], Acc) ->
     Acc.
   
reverse([H|T], Acc) ->
    reverse(T, [H|Acc]);
reverse([], Acc) ->
    Acc.