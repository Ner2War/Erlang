-module(p15).
-export([replicate/2]).
replicate(L,N) ->
    replicate(L,N,[],N).

replicate([H|T],N,Acc,Xn) ->
    head_replicate(H,T,N,Acc,Xn);
replicate([],_,Acc,_) ->
    p05:reverse(Acc).

head_replicate(_,T,0,Acc,Xn) ->
    replicate(T,Xn,Acc,Xn);
head_replicate(H,T,N,Acc,Xn) ->
    head_replicate(H,T,N-1,[H|Acc],Xn).