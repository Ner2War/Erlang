-module(tests).
-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).
last_test_() -> [
    ?_assert(p01:last([a,b,c,d]) =:= d),
    ?_assert(p01:last([a]) =:= a)
].

but_last_test_() -> [
    ?_assert(p02:but_last([a,b,c,d]) =:= [c,d]),
    ?_assert(p02:but_last([a,b]) =:= [a,b])
].

element_at_test_() -> [
    ?_assert(p03:element_at([a,b,c,d,e,f], 4) =:= d)
].

len_test_() -> [
    ?_assert(p04:len([]) =:= 0),
    ?_assert(p04:len([a,b,c,d]) =:= 4)
].

reverse_test_() -> [
    ?_assert(p05:reverse([1,2,3]) =:= [3,2,1])
].

is_palindrome_test_() -> [
    ?_assert(p06:is_palindrome([1,2,3,2,1]) =:= true),
    ?_assert(p06:is_palindrome([1,2,3]) =:= false)
].

flatten_test_() -> [
    ?_assert(p07:flatten([a,[],[b,[c,d],e]]) =:= [a,b,c,d,e])
].

compress_test_() -> [
    ?_assert(p08:compress([a,a,a,a,b,c,c,a,a,d,e,e,e,e]) =:= [a,b,c,a,d,e])
].

pack_test_() -> [
    ?_assert(p09:pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e]) =:= [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]])
].

encode_test_() -> [
    ?_assert(p10:encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e]) =:= [{4,a},{1,b},{2,c},{2,a},{1,d},{4,e}])
].

encode_modified_test_() -> [
    ?_assert(p11:encode_modified([a,a,a,a,b,c,c,a,a,d,e,e,e,e]) =:= [{4,a},b,{2,c},{2,a},d,{4,e}])
].

decode_modified_test_() -> [
    ?_assert(p12:decode_modified([{4,a},b,{2,c},{2,a},d,{4,e}]) =:= [a,a,a,a,b,c,c,a,a,d,e,e,e,e])
].

decode_test_() -> [
    ?_assert(p13:decode([{4,a},{1,b},{2,c},{2,a},{1,d},{4,e}]) =:= [a,a,a,a,b,c,c,a,a,d,e,e,e,e])
].

duplicate_test_() -> [
    ?_assert(p14:duplicate([a,b,c,c,d]) =:= [a,a,b,b,c,c,c,c,d,d])
].

replicate_test_() -> [
    ?_assert(p15:replicate([a,b,c], 3) =:= [a,a,a,b,b,b,c,c,c])
].
-endif.

