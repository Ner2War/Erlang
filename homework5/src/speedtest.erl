-module(speedtest).
-export([testpd/0, testmap/0, testdict/0,testets/0,testprop/0]).
-define(COUNT, 100000).
%%-compile(export_all).

testpd()->

%Process Dictionary
{Time1, _Value1} = timer:tc(fun() ->
    put(key1, val1)end), 
    io:format("Time to put to PD: ~p ~n ", [Time1]),
{Time2, _Value2} = timer:tc(fun() ->
    put(key2, val2)end), io:format("time needed to new put: ~p ~n ", [Time2]),
{Time3, _Value3} = timer:tc(fun() ->
    put(key2, change)end), 
    io:format("Time needed to change value: ~p ~n ", [Time3]),
{Time4, _Value4} = timer:tc(fun() ->
    get()end), 
    io:format("Time needed to get all value: ~p ~n ", [Time4]),
{Time5, _Value5} = timer:tc(fun() ->
    get(key1)end), 
    io:format("Time needed to get value: ~p ~n ", [Time5]),
{Time6, _Value6} = timer:tc(test100k, addpd, [?COUNT, 0]),
    io:format("Time needed(add ~p elements to PD): ~p ~n", [?COUNT,Time6]),
{Time7, _Value7} = timer:tc(fun() ->
    get(5000)end), 
    io:format("Result (time needed(read from ~p elements in PD): ~p ~n ", [?COUNT,Time7]).

%Maps
testmap() ->
{Time1, Value1} = timer:tc(fun() ->
    maps:new()end), io:format("Time needed to create Map: ~p ~n ", [Time1]),
{Time2, Value2} = timer:tc(fun() ->
    maps:put(key1, "value 1", Value1) end),
    io:format("Time needed to put value: ~p ~n ", [Time2]),
{Time3, Value3} = timer:tc(fun() ->
    maps:put(key1, "new value", Value2)end), 
    io:format("Time needed to change value: ~p ~n ", [Time3]),
{Time4, Value4} = timer:tc(fun() ->
    maps:update(key1, "new value2", Value3)end),
    io:format("Time needed to update value: ~p ~n ", [Time4]),
{Time5, _Value5} = timer:tc(fun() ->
    maps:get(key1, Value4)end),
    io:format("Time needed to get value: ~p ~n ", [Time5]),
{Time6, _Value6} = timer:tc(fun() ->
    maps:find(key1, Value4)end),
    io:format("Time needed to find value: ~p ~n ", [Time6]),
{Time7, _Value7} = timer:tc(fun() ->
    #{key1 := _Key} = Value4 end),
    io:format("Time needed to compare: ~p ~n ", [Time7]),
{Time8,Value8} = timer:tc(test100k, addmapread, [?COUNT, #{}]),
    io:format("time needed(add ~p elements to Maps): ~p ~n ", [?COUNT,Time8]),
{Time9,_Value9} = timer:tc(fun() ->
    maps:get(5000, Value8)end),
    io:format("time to read from ~p needed: ~p ~n ", [?COUNT,Time9]).

%DICT
testdict() ->
{Time1, Value1} = timer:tc(fun() ->
    dict:new()end),
    io:format("Time needed to create dict: ~p ~n ", [Time1]),
{Time2, Value2} = timer:tc(fun() ->
    dict:store(key1, "val 1", Value1)end),
    io:format("Time needed to add: ~p ~n ", [Time2]),
{Time3, Value3} = timer:tc(fun() ->
    dict:store(key1, "new val", Value2) end),
    io:format("Time needed to change value: ~p ~n ", [Time3]),
{Time4, _Value4} = timer:tc(fun() ->
    dict:fetch(key1, Value3)end),
    io:format("Time needed to find value: ~p ~n ", [Time4]),
{Time5, Value5} = timer:tc(test100k, addd, [?COUNT, dict:new()]),
    io:format("time needed(add ~p elements to Dict): ~p ~n ", [?COUNT,Time5]),
{Time6, _Value6} = timer:tc(fun() ->
    dict:fetch(5000, Value5)end),
    io:format("time needed to read from ~p Dict: ~p ~n ", [?COUNT,Time6]).

%ETS
testets() ->
{Time1, _Value1} = timer:tc(fun() ->
    ets:new(speed,[public,named_table])end),
    io:format("Time needed to create ETS: ~p ~n ", [Time1]),
{Time2, _Value2} = timer:tc(fun() ->
    ets:insert(speed, {1, "Bob", 25, undefined})end),
    io:format("Time needed to insert: ~p ~n ", [Time2]),
{Time3, _Value3} = timer:tc(fun() ->
    ets:lookup(speed, 1)end),
    io:format("Time needed to lookup: ~p ~n ", [Time3]),
{Time4, _Value4} = timer:tc(fun() ->
    ets:insert(speed, {1, "Helen A.", 21, undefined})end),
    io:format("Time needed to change value: ~p ~n ", [Time4]),
{Time5, _Value5} = timer:tc(fun() ->
    ets:match(speed, {'$1', '$2','$3', undefined})end),
    io:format("Time needed to match: ~p ~n ", [Time5]),
{Time6, Value6} = timer:tc(test100k, addetsk, [?COUNT, ets:new(tab,[public,named_table])]),
    io:format("time needed(add ~p elements to ETS): ~p ~n ", [?COUNT,Time6]),
{Time7,_Value7} = timer:tc(fun() ->
    ets:lookup(Value6, 5000)end),
    io:format("time needed to read from ~p ETS: ~p ~n ", [?COUNT,Time7]).

% proplist
testprop() ->
{Time1, Value1} = timer:tc(fun() ->
    [{key1,1},{key2,2},{key3,true}]end),
    io:format("Time needed to first add: ~p ~n ", [Time1]),
{Time2, Value2} = timer:tc(fun() ->
    [{key4,4},{key5,5}|Value1] end),
    io:format("Time needed to new add: ~p ~n ", [Time2]),
{Time3, Value3} = timer:tc(fun() ->
    [{key1,"New"} | Value2] end),
    io:format("Time needed to change value: ~p ~n ", [Time3]),
{Time4,_Value4} = timer:tc(fun() ->
    proplists:get_value(key1, Value3)end),
    io:format("Time needed to get value: ~p ~n ", [Time4]),
{Time5, Value5} = timer:tc(fun() ->
    [{_,_},{_,_},{_,_},{_,_},{_,_}]= Value2 end),
    io:format("Time needed to match: ~p ~n ", [Time5]),
{Time6,_Value6} = timer:tc(fun() ->
    proplists:lookup(key1, Value5)end),
    io:format("Time needed to lookup: ~p ~n ", [Time6]),
{Time7,Value7} = timer:tc(test100k, addprop, [?COUNT, []]),
    io:format("time needed(add ~p elements to PropList): ~p ~n ", [?COUNT,Time7]),
{Time8,_Value8} = timer:tc(fun() ->
    proplists:get_value(5000, Value7)end),
    io:format("Time needed to read from ~p PropList: ~p ~n ", [?COUNT,Time8]).
%%timer:tc(fun() -> io:format(“~p”, []) end)
%timer:tc(M, F, A)
