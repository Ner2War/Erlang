-module(my_cache).
-export([create/1, insert/4, lookup/2,delete_obsolete/1]).
-define(CURRENT_TIME, calendar:datetime_to_gregorian_seconds(calendar:local_time())).
-record(cache_lifetime, {named, object, life_time}).
-include_lib("stdlib/include/ms_transform.hrl").

create(TableName) ->
    _ = ets:new(TableName, [set, public, named_table, {keypos, 2}]),
    ok.

insert(TableName, Key, Value, Time) ->
    LifeTimer = ?CURRENT_TIME + Time,
    true = ets:insert(TableName, #cache_lifetime{named=Key, object=Value, life_time=LifeTimer}),
    ok.

lookup(TableName, Key) ->
    Time = ?CURRENT_TIME,
    case ets:lookup(TableName, Key) of
        [#cache_lifetime{object=Sell, life_time=LifeTime}] when LifeTime > Time ->
        {ok, Sell};
        _ -> {ok, []}                    
    end.

delete_obsolete(TableName) ->
    Time = ?CURRENT_TIME,
    MS = ets:fun2ms(fun(Record) when Time > Record#cache_lifetime.life_time ->
    true
    end),
    _= ets:select_delete(TableName, MS),ok.
