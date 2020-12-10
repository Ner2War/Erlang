-module(cache_server_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [{cache_server, {cache_server, start_link, [tablename, [{drop_interval, 3600}]]},permanent,5000, worker, [cache_server]}],
	{ok, {{one_for_one, 1, 5}, Procs}}.
