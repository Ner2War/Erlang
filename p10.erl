-module(p10).
-export([encode/1]).

encode([H|T]) ->
    encode (T,[{1,H}]);
encode([]) ->
    [].

encode([H|T],[{N,H}|CT]) ->
    encode(T,[{N+1,H}|CT]);
encode ([HN|T],[{N,H}|CT]) ->
    encode (T,[{1,HN}|[{N,H}|CT]]);
encode ([],CT) ->
    p05:reverse(CT).
    
%encode(L) ->
    %encode(L,1,[]).

%encode([H,H|T], Acc, Fin) ->
    %encode([H|T], Acc+1, Fin);
%encode([H,H2|T],Acc,Fin) ->
    %encode ([H2|T],1,[{Acc,H}|Fin]);
%encode ([H],Acc,Fin) ->
    %p05:reverse([{Acc,H}|Fin]).



