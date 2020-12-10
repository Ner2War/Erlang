-module (cache_server).
-export([start_link/2,init/1,handle_info/2,handle_call/3,insert/4,lookup/2,handle_cast/2,lookup_by_date/3]).
-define(CURRENT_TIME, calendar:datetime_to_gregorian_seconds(calendar:local_time())).
-behaviour(gen_server).
-record(cache_lifetime, {named, object, life_time,create}).
-include_lib("stdlib/include/ms_transform.hrl").

start_link(TableName, List) ->
	Drop = proplists:get_value(drop_interval, List),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [TableName,Drop], []).

init([TableName, Drop]) ->
	TableName = ets:new(TableName, [set, public, named_table, {keypos, 2}]),
	erlang:send_after(Drop, self(), delete_obsolete),
	State = {TableName, Drop},
	{ok, State}.

handle_info(delete_obsolete, {TableName, Drop}) ->
	Time = ?CURRENT_TIME,
    MS = ets:fun2ms(fun(Record) when Time > Record#cache_lifetime.life_time ->
    	true
    end),
    _= ets:select_delete(TableName, MS),
	erlang:send_after(Drop, self(), delete_obsolete),
	NewState = {TableName, Drop},
	{noreply, NewState};
handle_info(_Info, State) ->
	{noreply, State}.

handle_call({insert,Key, Value, Time},_,{TableName, Drop}) ->
 	LifeTimer = ?CURRENT_TIME + Time,
	CreateTime = ?CURRENT_TIME,
	     true = ets:insert(TableName, #cache_lifetime{named=Key, object=Value, life_time=LifeTimer,create=CreateTime}),
	NewState={TableName, Drop},
	{reply, ok, NewState};
handle_call({lookup, Key},_,{TableName, _Drop}=State) ->
	Time = ?CURRENT_TIME,
    Result = case ets:lookup(TableName, Key) of
    [#cache_lifetime{object=Sell, life_time=LifeTime}] when LifeTime > Time ->
    {ok, Sell};
    _ -> {ok, []}                    
    end, 
	{reply, Result, State};
handle_call({lookupdate,DateFrom,DateTo},_,{TableName,_Drop}=State) ->
	DateFromS = calendar:datetime_to_gregorian_seconds(DateFrom),
	DateToS = calendar:datetime_to_gregorian_seconds(DateTo),
	MS = ets:fun2ms(fun(Record) when 
	DateFromS < Record#cache_lifetime.create, Record#cache_lifetime.create < DateToS ->
		{Record#cache_lifetime.named, Record#cache_lifetime.object}
	end),
	Result= ets:select(TableName,MS), 
	{reply, {ok,[[{<<"key">>,X},{<<"value">>,Y}] || {X,Y} <- Result]}, State}.
   
insert(_TableName, Key, Value, Time) ->
	InsertData = {insert, Key, Value, Time},
	gen_server:call(?MODULE, InsertData).

lookup(_TableName, Key) ->
	LookUpData= {lookup,Key},
    gen_server:call(?MODULE,LookUpData).

handle_cast(_Msg, State) ->
	{noreply, State}.

lookup_by_date(_TableName,DateFrom,DateTo) ->
	LookupDate = {lookupdate,DateFrom,DateTo},
	gen_server:call(?MODULE,LookupDate).

