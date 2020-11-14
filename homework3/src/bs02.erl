-module(bs02).
-export([words/1]).
words(B) ->
		words(B, <<>>, []).
		
words(<<" ",Rest/binary >> , Acc,Fin) ->
		words(Rest,<<>>,[Acc|Fin]);
words(<<H/utf8,Rest/binary>>, Acc,Fin) ->
		words(Rest, <<Acc/binary, H/utf8>>,Fin);
words(<<>>, Acc, Fin) ->
		lists:reverse([Acc|Fin]).