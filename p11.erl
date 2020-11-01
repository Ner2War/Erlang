-module(p11).
-export([encode_modified/1]).
encode_modified(L) ->
    encode_modified(L,1,[]).

encode_modified([H,H|T], Acc, Fin) ->
    encode_modified([H|T], Acc+1, Fin);
encode_modified([H,H2|T],1,Fin) ->
    encode_modified ([H2|T],1,[H|Fin]);
encode_modified([H,H2|T],Acc,Fin) ->
    encode_modified ([H2|T],1,[{Acc,H}|Fin]);
encode_modified([H],1,Fin) ->   
    reverse([H|Fin],[]);
encode_modified ([H],Acc,Fin) ->
    reverse([{Acc,H}|Fin],[]).

reverse([H|T], Acc) ->
    reverse(T, [H|Acc]);
reverse([], Acc) ->
    Acc.