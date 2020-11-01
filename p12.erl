-module(p12).
-export([decode_modified/1]).
decode_modified(L) ->
    decode_modified(L,[]).
    


decode_modified([{N,H}|T],Acc) ->
    unpack ({N,H},T,Acc);
decode_modified([H|T],Acc) ->
    decode_modified(T,[H|Acc]);
decode_modified([],Acc) ->
    reverse(Acc,[]).

unpack({1,H},T,Acc) ->
     decode_modified(T,[H|Acc]);
unpack({N,H},T,Acc) ->
    unpack ({N-1,H},T,[H|Acc]).

reverse([H|T], Acc) ->
    reverse(T, [H|Acc]);
reverse([], Acc) ->
    Acc.
