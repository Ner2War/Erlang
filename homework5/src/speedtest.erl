-module(speedtest).
-export([testpd/0, testmap/0, testdict/0,testets/0,testprop/0]).
%%-compile(export_all).

testpd()->

%Process Dictionary
{Time1,Value1} = timer:tc(fun() ->
    put(key1,val1)end), io:format("Result: ~p, time needed: ~p ", [Value1, Time1]),
{Time2,Value2} = timer:tc(fun() ->
    put(key2,val2)end), io:format("Result: ~p, time needed: ~p ", [Value2, Time2]),
{Time3,Value3} = timer:tc(fun() ->
    put(key2,change)end), io:format("Result: ~p, time needed: ~p ", [Value3, Time3]),
{Time4,Value4} = timer:tc(fun() ->
    get()end), io:format("Result: ~p, time needed: ~p ", [Value4, Time4]),
{Time5,Value5} = timer:tc(fun() ->
    get(key1)end), io:format("Result: ~p, time needed: ~p ", [Value5, Time5]).

%Maps
testmap() ->
{Time1,Value1} = timer:tc(fun() ->
     maps:new()end), io:format("Result: ~p, time needed: ~p ", [Value1, Time1]),
{Time2,Value2} = timer:tc(fun() ->
     maps:put(key1, "value 1", Value1) end),io:format("Result: ~p, time needed: ~p ", [Value2, Time2]),
{Time3,Value3} = timer:tc(fun() ->
    maps:put(key1, "new value", Value2)end), io:format("Result: ~p, time needed: ~p ", [Value3, Time3]),
{Time4,Value4} = timer:tc(fun() ->
     maps:update(key1, "new value2", Value3)end), io:format("Result: ~p, time needed: ~p ", [Value4, Time4]),
{Time5,Value5} = timer:tc(fun() ->
    maps:get(key1, Value4)end), io:format("Result: ~p, time needed: ~p ", [Value5, Time5]),
{Time6,Value6} = timer:tc(fun() ->
    maps:find(key1, Value4)end), io:format("Result: ~p, time needed: ~p ", [Value6, Time6]),
{Time7,Value7} = timer:tc(fun() ->
#{key1 := _Key} = Value4 end), io:format("Result: ~p, time needed: ~p ", [Value7, Time7]).

%DICT
testdict() ->
{Time1,Value1} = timer:tc(fun() ->
     dict:new()end), io:format("Result: ~p, time needed: ~p ", [Value1, Time1]),
{Time2,Value2} = timer:tc(fun() ->
     dict:store(key1, "val 1", Value1)end), io:format("Result: ~p, time needed: ~p ", [Value2, Time2]),
{Time3,Value3} = timer:tc(fun() ->
     dict:store(key1, "new val", Value2) end), io:format("Result: ~p, time needed: ~p ", [Value3, Time3]),
{Time4,Value4} = timer:tc(fun() ->
    dict:fetch(key1, Value3)end), io:format("Result: ~p, time needed: ~p ", [Value4, Time4]).

%ETS
testets() ->
{Time1,Value1} = timer:tc(fun() ->
    ets:new(speed,[public,named_table])end), io:format("Result: ~p, time needed: ~p ", [Value1, Time1]),
{Time2,Value2} = timer:tc(fun() ->
    ets:insert(speed, {1, "Bob", 25, undefined})end), io:format("Result: ~p, time needed: ~p ", [Value2, Time2]),
{Time3,Value3} = timer:tc(fun() ->
    ets:lookup(speed, 1)end),  io:format("Result: ~p, time needed: ~p ", [Value3, Time3]),
{Time4,Value4} = timer:tc(fun() ->
    ets:insert(speed, {1, "Helen A.", 21, undefined})end), io:format("Result: ~p, time needed: ~p ", [Value4, Time4]),
{Time5,Value5} = timer:tc(fun() ->
    ets:match(speed, {'$1', '$2','$3', undefined})end), io:format("Result: ~p, time needed: ~p ", [Value5, Time5]).

% proplist
testprop() ->
{Time1,Value1} = timer:tc(fun() ->
    [{key1,1},{key2,2},{key3,true}]end), io:format("Result: ~p, time needed: ~p ", [Value1, Time1]),
{Time2,Value2} = timer:tc(fun() ->
    [{key4,4},{key5,5}|Value1] end),io:format("Result: ~p, time needed: ~p ", [Value2, Time2]),
{Time3,Value3} = timer:tc(fun() ->
    [{key1,"New"} | Value2] end), io:format("Result: ~p, time needed: ~p ", [Value3, Time3]),
{Time4,Value4} = timer:tc(fun() ->
    proplists:get_value(key1, Value3)end), io:format("Result: ~p, time needed: ~p ", [Value4, Time4]),
{Time5,Value5} = timer:tc(fun() ->
    [{_,_},{_,_},{_,_},{_,_},{_,_}]= Value2 end), io:format("Result: ~p, time needed: ~p ", [Value5, Time5]),
{Time6,Value6} = timer:tc(fun() ->
   proplists:lookup(key1, Value5)end), io:format("Result: ~p, time needed: ~p ", [Value6, Time6]).
%%timer:tc(fun() -> io:format(“~p”, []) end)
%timer:tc(M, F, A)
