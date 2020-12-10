-module(pars).
-export([decode/2]).

decode(<<"{",Json/binary>>,proplist)->
        {_Rest,ParsedJson}=key_val_parse(Json,<<>>,[],proplist),
        ParsedJson;
decode(<<"{",Json/binary>>, map)->
        {_Rest,ParsedJson}=key_val_parse(Json,<<>>,#{},map),
        ParsedJson;
decode(<<X,Json/binary>>, Type) when X==$\n; X==$\s ->
        decode(Json, Type).

key_val_parse(<<X,Bin/binary>>,Key,Acc,Type)when X==$,;X==$\s; X==$\n ; X==$:->
        key_val_parse(Bin,Key,Acc,Type);
key_val_parse(<<"\"",_Rest/binary>>=Val,<<>>,Acc,Type) ->
        {Rest,Value}=parse_value(Val,Type),
        key_val_parse(Rest,Value,Acc,Type);
key_val_parse(<<"}",Rest/binary>>,_Key,Acc,proplist) ->
        {Rest,lists:reverse(Acc)};
key_val_parse(<<"}",Rest/binary>>,_Key,Acc,map)  ->
        {Rest,Acc};
key_val_parse(Bin,Key,Acc, Type) ->
        {Rest,Value}=parse_value(Bin,Type),
        key_val_parse(Rest,<<>>,construct_acc(Key, Value, Acc, Type), Type).

construct_acc(Key, Value, Acc, map) ->
        maps:put(Key,Value,Acc);
construct_acc(Key, Value, Acc, proplist) ->
        [{Key, Value}|Acc].

parse_value(<<X/utf8, _Rest/binary>>=Binary,_Type) when X>=$0, X=<$9->
        parse_num(Binary,<<>>);
parse_value(<<"\"", Rest/binary>>,_Type) ->
        parse_string(Rest,<<>>);
parse_value(<<"true",Rest/binary>>,_Type) ->
        {Rest,true};
parse_value(<<"false",Rest/binary>>,_Type) ->
        {Rest,false};
parse_value(<<"[",Bin/binary>>,Type) ->
        massiv_parse(Bin,[],Type);
parse_value(<<"{",Bin/binary>>,proplist)  ->
        key_val_parse(Bin,<<>>,[],proplist);
parse_value(<<"{",Bin/binary>>,map)  ->
        key_val_parse(Bin,<<>>,#{},map).    

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

massiv_parse(<<"]", Rest/binary>>,Acc,_Type) ->
        {Rest,lists:reverse(Acc)};
massiv_parse(<<X, Rest/binary>>, Acc,Type)when X==$,;X==$\s; X==$\n ->
        massiv_parse(Rest, Acc,Type);
massiv_parse(<<"[", InsideMass/binary>>,Acc,Type) ->
        {Rest, Massiv} = massiv_parse(InsideMass,[],Type),
        massiv_parse(Rest, [Massiv|Acc],Type);
massiv_parse(Val,Acc,Type) ->
        {Rest,Value} = parse_value(Val,Type),
        massiv_parse(Rest,[Value|Acc],Type).
