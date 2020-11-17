-module(tests).
-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

first_word_test() -> [
    ?_assert(bs01:first_word(<<"Some text">>) =:= <<"Some">>)
].

words_test() -> [
    ?_assert(bs02:words(<<"Text with four words">>) =:= [<<"Text">>, <<"with">>, <<"four">>, <<"words">>])
].

split_test() -> [
    ?_assert(bs03:split(<<"Col1-:-Col2-:-Col3-:-Col4-:-Col5">>, "-:-") =:= [<<"Col1">>, <<"Col2">>, <<"Col3">>, <<"Col4">>, <<"Col5">>])
].

decode_test_() -> [
    ?_assert(bs04:decode(<<"{
 \"squadName\": \"Super hero squad\",
 \"homeTown\": \"Metro City\",
 \"formed\": 2016,
 \"secretBase\": \"Super tower\",
 \"active\": true,
 \"members\": [
 {
 \"name\": \"Molecule Man\",
 \"age\": 29,
 \"secretIdentity\": \"Dan Jukes\",
 \"powers\": [
 \"Radiation resistance\",
 \"Turning tiny\",
 \"Radiation blast\"
 ]
 },
 {
 \"name\": \"Madame Uppercut\",
 \"age\": 39,
 \"secretIdentity\": \"Jane Wilson\",
 \"powers\": [
 \"Million tonne punch\",
 \"Damage resistance\",
 \"Superhuman reflexes\"
 ]
 },
 {
 \"name\": \"Eternal Flame\",
 \"age\": 1000000,
 \"secretIdentity\": \"Unknown\",
 \"powers\": [
 \"Immortality\",
 \"Heat Immunity\",
 \"Inferno\",
 \"Teleportation\",
 \"Interdimensional travel\"
 ]
 }
 ]
 }">>, proplist) =:= [{<<"squadName">>,<<"Super hero squad">>},
 {<<"homeTown">>,<<"Metro City">>},
 {<<"formed">>,2016},
 {<<"secretBase">>,<<"Super tower">>},
 {<<"active">>,true},
 {<<"members">>,
  [[{<<"name">>,<<"Molecule Man">>},
    {<<"age">>,29},
    {<<"secretIdentity">>,<<"Dan Jukes">>},
    {<<"powers">>,
     [<<"Radiation resistance">>,<<"Turning tiny">>,
      <<"Radiation blast">>]}],
   [{<<"name">>,<<"Madame Uppercut">>},
    {<<"age">>,39},
    {<<"secretIdentity">>,<<"Jane Wilson">>},
    {<<"powers">>,
     [<<"Million tonne punch">>,<<"Damage resistance">>,
      <<"Superhuman reflexes">>]}],
   [{<<"name">>,<<"Eternal Flame">>},
    {<<"age">>,1000000},
    {<<"secretIdentity">>,<<"Unknown">>},
    {<<"powers">>,
     [<<"Immortality">>,<<"Heat Immunity">>,<<"Inferno">>,
      <<"Teleportation">>,<<"Interdimensional travel">>]}]]}])
].


-endif.
