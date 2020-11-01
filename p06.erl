-module(p06).
-export([is_palindrome/1]).
is_palindrome(L) ->
    is_palindrome (L,reverse(L,[])).
    
is_palindrome(L,L) ->
    true;
is_palindrome(_,_) ->
    false.


reverse([H|T], Acc) ->
    reverse(T, [H|Acc]);
reverse([], Acc) ->
    Acc.
