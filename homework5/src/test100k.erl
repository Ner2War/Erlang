-module(test100k).
-export([addval/3,addprop/2,addets/3,addetsk/2, addict/3, addd/2, addmap/3,addmapread/2,addpd/2]).
%%-compile(export_all).

%проплисты
addval(0,Val,Time) ->
    {Val, Time};
addval(A,Val,Time) -> 
    {T,Value} = timer:tc(fun() -> [{A, 6}|Val] end),
      addval(A-1, Value, Time + T).

addprop(0,Val) ->
    Val;
addprop(A,Val) ->
    addprop(A-1,[{A,1}|Val]).
%ETS
addets(0,tab,Time)->
    {tab,Time};
addets(A,tab,Time) ->
    {T,_Value} = timer:tc(fun() -> true=ets:insert(tab, {A, value1, value2, value3}) end),
      addets(A-1, tab, Time + T).

addetsk(0,Tab) ->
    Tab;
addetsk(A,Tab) ->
    true=ets:insert(Tab, {A, value1}),
    addetsk(A-1, Tab).

%DICT
addict(0,Dict,Time) ->
    {Dict,Time};
addict(A,Dict,Time) ->
    {T,Value} = timer:tc(fun() -> dict:store(A, "val 1", Dict) end),
    addict(A-1,Value, Time + T).

addd(0,Dict) ->
    Dict;
addd(A,Dict) ->
    addd(A-1, dict:store(A, "val 1", Dict)).

%Maps
addmap(0,Map,Time) ->
    {Map,Time};
addmap(A,Map,Time) ->
    {T,Value} = timer:tc(fun() -> maps:put(A,9,Map)end),
    addmap(A-1,Value,Time+T).

addmapread(0,Map) -> Map;
addmapread(A,Map) ->
addmapread(A-1,maps:put(A,6,Map)).

%Process Dictionary
addpd(0,Time) ->
    Time;
addpd(A,Time) ->
    {T,_Value} = timer:tc(fun() -> put(A,9)end),
    addpd(A-1,Time+T).

