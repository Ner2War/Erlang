-module(p09).
-export([pack/1]).

pack([H|T]) ->
    pack(T,[[H]]);
pack([]) ->
    [].

pack([H|T],[HT=[H|_]|AccT]) ->
    pack(T,[[H|HT]|AccT]);
pack([H|T],[HT=[_|_]|AccT]) ->
    pack(T,[[H]|[HT|AccT]]);
pack([],AccT) ->
    p05:reverse(AccT).



%%pack(L) ->
    %pack(L,[],[]).

%pack([H,H|T], Acc, Fin) ->
    %pack([H|T],[H|Acc],Fin);
%pack([H,H2|T],Acc,Fin) ->
    %pack ([H2|T],[],[[H|Acc]|Fin]);
%pack ([H],Acc,Fin) ->
    %p05:reverse([[H|Acc]|Fin]).

