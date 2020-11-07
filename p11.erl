-module(p11).
-export([encode_modified/1]).

encode_modified([H|T]) ->
    encode_modified(T,[{1,H}]);
encode_modified([]) ->
    [].

encode_modified([H|T],[{N,H}|TAcc]) ->
    encode_modified(T,[{N+1,H}|TAcc]);
encode_modified([H|T],[{1,Hn}|TAcc]) ->
    encode_modified(T,[{1,H}|[Hn|TAcc]]);
encode_modified([H|T],[HAcc={_,_}|TAcc]) ->
    encode_modified(T,[{1,H}|[HAcc|TAcc]]);
encode_modified([],Tacc) ->
    p05:reverse(Tacc).



%%encode_modified(L) ->
    %%encode_modified(L,1,[]).

%encode_modified([H,H|T], Acc, Fin) ->
    %encode_modified([H|T], Acc+1, Fin);
%encode_modified([H,H2|T],1,Fin) ->
    %encode_modified ([H2|T],1,[H|Fin]);
%encode_modified([H,H2|T],Acc,Fin) ->
    %encode_modified ([H2|T],1,[{Acc,H}|Fin]);
%encode_modified([H],1,Fin) ->   
    %p05:reverse([H|Fin]);
%encode_modified ([H],Acc,Fin) ->
    %p05:reverse([{Acc,H}|Fin]).

