-module(bs03).
-export([split/2]).

split(B,G) ->
    Sep=list_to_binary(G),
    SepSize= byte_size(Sep),
        split(B, Sep, SepSize, [<<>>]).

split(<<>>,_,_,Acc) ->
    lists:reverse(Acc);
split(Bin,Sep,SepSize,[H|T]=Acc)->
    case Bin of
<<Sep:SepSize/binary,Begin/utf8,Rest/binary>> ->
    split(Rest,Sep,SepSize,[<<Begin>>|Acc]);
<<X/utf8,Rest/binary>> ->
    split(Rest,Sep,SepSize, [<<H/binary,X/utf8>>|T])
end.

%split(<<Sep:Size/binary, Rest/binary>>,Sep,Size,Acc) ->
%    split(Rest,Sep,Size,[<<>>|Acc]);
%split (<<X/utf8,Rest/binary>>,Sep,Size,[Acc|BT]) ->
%    split(Rest,Sep,Size,[<<Acc/binary,X/utf8>>|BT]);
%split (<<>>,_Sep,_Size,Acc) -> 
%    Acc.
