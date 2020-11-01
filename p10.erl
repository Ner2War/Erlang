-module(p10).
-export([encode/1]).
encode(L) ->
    encode(L,1,[]).

encode([H,H|T], Acc, Fin) ->
    encode([H|T], Acc+1, Fin);
encode([H,H2|T],Acc,Fin) ->
    encode ([H2|T],1,[{Acc,H}|Fin]);
encode ([H],Acc,Fin) ->
    reverse([{Acc,H}|Fin],[]).

reverse([H|T], Acc) ->
    reverse(T, [H|Acc]);
reverse([], Acc) ->
    Acc.