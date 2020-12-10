-module(cache_handler).
-export([init/2,bintime/1]).
-define(LIFE, 600).
-define(TABL, tablename).

init(Req0 = #{method := <<"POST">>}, State) ->
    {ok, DataBin, _Req} = cowboy_req:read_body(Req0),
    Data = jsx:decode(DataBin),
    Answer = case maps:get(<<"action">>, Data) of
        <<"insert">> -> 
            #{<<"key">> := Key, <<"value">> := Value} = Data,
            cache_server:insert(?TABL, Key, Value, ?LIFE),
            #{<<"result">> => <<"ok">>};
        <<"lookup">> -> 
            #{<<"key">> := Key} = Data,
            {ok, Val} = cache_server:lookup(?TABL, Key),
            #{<<"result">> => Val};
        <<"lookup_by_date">> -> 
            #{<<"date_from">> := DateFrom , <<"date_to">> := DateTo} = Data,
            {ok, Val} = cache_server:lookup_by_date(?TABL, bintime(DateFrom), bintime(DateTo)), 
            #{<<"result">> => Val}
    end,    
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, jsx:encode(Answer), Req0),
    {ok,Req,State};
init(Req0, State) ->
    Req = cowboy_req:reply(405, #{<<"content-type">> => <<"text/plain">>},<<"Use POST">>, Req0),
    {ok, Req, State}.

bintime(DateFrom) ->
	[Date,Time]=binary:split(DateFrom, <<" ">>,[global]),
	DateV = binary:split(Date,<<"/">>,[global]),
	TimeV = binary:split(Time,<<":">>,[global]),
	list_to_tuple([list_to_tuple([binary_to_integer(X) || X <- List])|| List <- [DateV,TimeV]]).