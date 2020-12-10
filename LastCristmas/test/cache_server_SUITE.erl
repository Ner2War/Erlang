-module(cache_server_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
    [{group,test}].

groups() ->
    [
    {test, [sequence], [
        start,
        insert,
        lookup,
        lookup_by_date
    ]}].

start(_Config) ->
    {ok, _Pid} = cache_server:start_link(test, [{drop_interval, 3600}]).
    
insert(_Config) ->
    {ok, _Pid} = cache_server:start_link(test, [{drop_interval, 3600}]),
    ok = cache_server:insert(test, key1, value1, 60),
    ok = cache_server:insert(test, key2, value2, 600),
    ok = cache_server:insert(test, key3, valye3, 6000).

lookup(_Config) ->
    {ok, _Pid} = cache_server:start_link(test, [{drop_interval, 3600}]),
    ok = cache_server:insert(test, key1, value1, 60),
    ok = cache_server:insert(test, key2, value2, 600),
    ok = cache_server:insert(test, key3, value3, 6000),
    {ok, value1} = cache_server:lookup(test, key1),
    {ok, value2} = cache_server:lookup(test, key2),
    {ok, value3} = cache_server:lookup(test, key3).

lookup_by_date(_Config) ->
    {ok, _Pid} = cache_server:start_link(test, [{drop_interval, 3600}]),
    ok = cache_server:insert(test, key1, value1, 60),
    DateFrom = {{2020,1,1},{00,00,00}},
    DateTo = {{2021,1,10},{23,59,59}},
    {ok, [value1]} = cache_server:lookup_by_date(test, DateFrom, DateTo).


