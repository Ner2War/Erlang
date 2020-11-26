-module(speedtest).
-export([testpd/0, testmap/0, testdict/0,testets/0,testprop/0]).
%%-compile(export_all).

testpd()->

%Process Dictionary
{Time1,_Value1} = timer:tc(fun() ->
    put(key1,val1)end), io:format("Time to put to PD: ~p ", [Time1]),
{Time2,_Value2} = timer:tc(fun() ->
    put(key2,val2)end), io:format("time needed to new put: ~p ", [Time2]),
{Time3,_Value3} = timer:tc(fun() ->
    put(key2,change)end), io:format("Time needed to change value: ~p ", [Time3]),
{Time4,_Value4} = timer:tc(fun() ->
    get()end), io:format("Time needed to get all value: ~p ", [Time4]),
{Time5,_Value5} = timer:tc(fun() ->
    get(key1)end), io:format("Time needed to get value: ~p ", [Time5]),
{Time6,_Value6} = timer:tc(test100k, addpd, [100000, 0]),
    io:format("Time needed(add 100k elements to PD): ~p ", [Time6]),
{Time7,_Value7} = timer:tc(fun() ->
    get(5000)end), io:format("Result (time needed(read 100k elements in PD): ~p ", [Time7]).

%Maps
testmap() ->
{Time1,Value1} = timer:tc(fun() ->
     maps:new()end), io:format("Time needed to create Map: ~p ", [Time1]),
{Time2,Value2} = timer:tc(fun() ->
     maps:put(key1, "value 1", Value1) end),io:format("Time needed to put value: ~p ", [Time2]),
{Time3,Value3} = timer:tc(fun() ->
    maps:put(key1, "new value", Value2)end), io:format("Time needed to change value: ~p ", [Time3]),
{Time4,Value4} = timer:tc(fun() ->
     maps:update(key1, "new value2", Value3)end), io:format("Time needed to update value: ~p ", [Time4]),
{Time5,_Value5} = timer:tc(fun() ->
    maps:get(key1, Value4)end), io:format("Time needed to get value: ~p ", [Time5]),
{Time6,_Value6} = timer:tc(fun() ->
    maps:find(key1, Value4)end), io:format("Time needed to find value: ~p ", [Time6]),
{Time7,_Value7} = timer:tc(fun() ->
#{key1 := _Key} = Value4 end), io:format("Time needed to compare: ~p ", [Time7]),
{Time8,Value8} = timer:tc(test100k, addmapread, [100000, #{}]),
    io:format("time needed(add 100k elements to Maps): ~p ", [Time8]),
{Time9,_Value9} = timer:tc(fun() ->
    maps:get(5000, Value8)end), io:format("time to read from 100k needed: ~p ", [Time9]).

%DICT
testdict() ->
{Time1,Value1} = timer:tc(fun() ->
     dict:new()end), io:format("Time needed to create dict: ~p ", [Time1]),
{Time2,Value2} = timer:tc(fun() ->
     dict:store(key1, "val 1", Value1)end), io:format("Time needed to add: ~p ", [Time2]),
{Time3,Value3} = timer:tc(fun() ->
     dict:store(key1, "new val", Value2) end), io:format("Time needed to change value: ~p ", [Time3]),
{Time4,_Value4} = timer:tc(fun() ->
    dict:fetch(key1, Value3)end), io:format("Time needed to find value: ~p ", [Time4]),
{Time5,Value5} = timer:tc(test100k, addd, [100000, dict:new()]),
    io:format("time needed(add 100k elements to Dict): ~p ", [Time5]),
{Time6,_Value6} = timer:tc(fun() ->
    dict:fetch(5000, Value5)end), io:format("time needed to read from 100k Dict: ~p ", [Time6]).

%ETS
testets() ->
{Time1,_Value1} = timer:tc(fun() ->
    ets:new(speed,[public,named_table])end), io:format("Time needed to create ETS: ~p ", [Time1]),
{Time2,_Value2} = timer:tc(fun() ->
    ets:insert(speed, {1, "Bob", 25, undefined})end), io:format("Time needed to insert: ~p ", [Time2]),
{Time3,_Value3} = timer:tc(fun() ->
    ets:lookup(speed, 1)end),  io:format("Time needed to lookup: ~p ", [Time3]),
{Time4,_Value4} = timer:tc(fun() ->
    ets:insert(speed, {1, "Helen A.", 21, undefined})end), io:format("Time needed to change value: ~p ", [Time4]),
{Time5,_Value5} = timer:tc(fun() ->
    ets:match(speed, {'$1', '$2','$3', undefined})end), io:format("Time needed to match: ~p ", [Time5]),
{Time6,Value6} = timer:tc(test100k, addetsk, [100000, ets:new(tab,[public,named_table])]),
    io:format("time needed(add 100k elements to ETS): ~p ", [Time6]),
{Time7,_Value7} = timer:tc(fun() ->
    ets:lookup(Value6, 5000)end),  io:format("time needed to read from 100k ETS: ~p ", [Time7]).

% proplist
testprop() ->
{Time1,Value1} = timer:tc(fun() ->
    [{key1,1},{key2,2},{key3,true}]end), io:format("Time needed to first add: ~p ", [Time1]),
{Time2,Value2} = timer:tc(fun() ->
    [{key4,4},{key5,5}|Value1] end),io:format("Time needed to new add: ~p ", [Time2]),
{Time3,Value3} = timer:tc(fun() ->
    [{key1,"New"} | Value2] end), io:format("Time needed to change value: ~p ", [Time3]),
{Time4,_Value4} = timer:tc(fun() ->
    proplists:get_value(key1, Value3)end), io:format("Time needed to get value: ~p ", [Time4]),
{Time5,Value5} = timer:tc(fun() ->
    [{_,_},{_,_},{_,_},{_,_},{_,_}]= Value2 end), io:format("Time needed to match: ~p ", [Time5]),
{Time6,_Value6} = timer:tc(fun() ->
    proplists:lookup(key1, Value5)end), io:format("Time needed to lookup: ~p ", [Time6]),
{Time7,Value7} = timer:tc(test100k, addprop, [100000, []]),
    io:format("time needed(add 100k elements to PropList): ~p ", [Time7]),
{Time8,_Value8} = timer:tc(fun() ->
    proplists:get_value(5000, Value7)end), io:format("Time needed ro read from 100k PropList: ~p ", [Time8]).
%%timer:tc(fun() -> io:format(“~p”, []) end)
%timer:tc(M, F, A)
