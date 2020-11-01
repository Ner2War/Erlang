-module(p13).
-export([decode/1]).
decode(L) ->
    decode(L,[]).

decode([{N,H}|T],Acc) ->
    unpack ({N,H},T,Acc);
decode([],Acc) ->
    reverse(Acc,[]).

unpack({1,H},T,Acc) ->
     decode(T,[H|Acc]);
unpack({N,H},T,Acc) ->
    unpack ({N-1,H},T,[H|Acc]).

reverse([H|T], Acc) ->
    reverse(T, [H|Acc]);
reverse([], Acc) ->
    Acc.
