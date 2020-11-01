-module(p08).
-export([compress/1]).
compress(L) ->
    compress(L, []).

compress([H, H| T], Acc) ->
    compress([H| T], Acc);
compress([H, H2| T], Acc) ->
    compress([H2| T],[H| Acc]);
compress([H], Acc) ->
     reverse([H| Acc],[]).

reverse([H|T], Acc) ->
    reverse(T, [H|Acc]);
reverse([], Acc) ->
    Acc.

