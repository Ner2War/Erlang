-module(p06).
-export([is_palindrome/1]).
is_palindrome(L) ->
    is_palindrome (L,p05:reverse(L)).
   
is_palindrome(L,L) ->
    true;
is_palindrome(_,_) ->
    false.


