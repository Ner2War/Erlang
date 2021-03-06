-module(bs04map).
-export([decode/2]).

decode(<<"{",Json/binary>>, map)->
    {_Rest,ParsedJson}=key_val_parse(Json,<<>>,#{}),
    ParsedJson;
decode(<<X,Json/binary>>, map) when X==$\n; X==$\s ->
    decode(Json, map).

key_val_parse(<<X,Bin/binary>>,Key,Map)when X==$,;X==$\s; X==$\n ; X==$:->
    key_val_parse(Bin,Key,Map);
key_val_parse(<<"\"",_Rest/binary>>=Val,<<>>,Map) ->
    {Rest,Value}=parse_value(Val),
    key_val_parse(Rest,Value,Map);
key_val_parse(<<"}",Rest/binary>>,_Key,Map) ->
    {Rest,Map};
key_val_parse(Bin,Key,Map) ->
    {Rest,Value}=parse_value(Bin),
    key_val_parse(Rest,<<>>, maps:put(Key,Value,Map)).

parse_value(<<X/utf8, _Rest/binary>>=Binary) when X>=$0, X=<$9->
    parse_num(Binary,<<>>);
parse_value(<<"\"", Rest/binary>>) ->
    parse_string(Rest,<<>>);
parse_value(<<"true",Rest/binary>>) ->
    {Rest,true};
parse_value(<<"false",Rest/binary>>) ->
    {Rest,false};
parse_value(<<"[",Bin/binary>>) ->
    massiv_parse(Bin,[]);
parse_value(<<"{",Bin/binary>>) ->
    key_val_parse(Bin,<<>>,#{}).

parse_num(<<X/utf8,Rest/binary>>,Acc) when X>=$0, X=<$9 ->
    parse_num(Rest,<<Acc/binary,X/utf8>>);
parse_num(<<" ",_Rest/binary>>,Acc) ->
    parse_num(_Rest,Acc);
parse_num(<<_/utf8, _/binary>>=Rest,Acc) ->
    {Rest,binary_to_integer(Acc)}.

parse_string(<<"\"",Rest/binary>>,Acc) ->
    {Rest,Acc};
parse_string(<<X/utf8,Rest/binary>>,Acc) ->
    parse_string(Rest,<<Acc/binary, X/utf8>>).

massiv_parse(<<"]", Rest/binary>>,Acc) ->
    {Rest,lists:reverse(Acc)};
massiv_parse(<<X, Rest/binary>>, Acc)when X==$,;X==$\s; X==$\n ->
    massiv_parse(Rest, Acc);
massiv_parse(<<"[", InsideMass/binary>>,Acc) ->
    {Rest, Massiv} = massiv_parse(InsideMass,[]),
    massiv_parse(Rest, [Massiv|Acc]);
massiv_parse(Val,Acc) ->
    {Rest,Value} = parse_value(Val),
    massiv_parse(Rest,[Value|Acc]).