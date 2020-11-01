-module(p09).
-export([pack/1]).
pack(L) ->
    pack(L,[],[]).

pack([H,H|T], Acc, Fin) ->
    pack([H|T],[H|Acc],Fin);
pack([H,H2|T],Acc,Fin) ->
    pack ([H2|T],[],[[H|Acc]|Fin]);
pack ([H],Acc,Fin) ->
    reverse([[H|Acc]|Fin],[]).

reverse([H|T], Acc) ->
    reverse(T, [H|Acc]);
reverse([], Acc) ->
    Acc.
