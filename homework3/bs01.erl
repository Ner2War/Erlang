-module(bs01).
-export([first_word/1]).
first_word(B) ->
		first_word(B, <<>>).
		
first_word(<< " ",_Rest/binary >> , Acc) ->
		Acc;
first_word(<<H/utf8,Rest/binary>>, Acc) ->
		first_word(Rest, <<Acc/binary, H/utf8>>);
first_word(<<>>, Acc) ->
		Acc.