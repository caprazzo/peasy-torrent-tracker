-module(peasy_supervisor).

-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Port) ->
	supervisor:start_link({local,?MODULE}, ?MODULE, [Port]).

init([Port]) ->
	process_flag(trap_exit, true),
	io:format("~p (~p) starting...~n", [?MODULE, self()]),
	Web = {peasy_web, {peasy_web, start_link, [Port]},
		   permanent, 5000, worker, [peasy_web]},
	Db  = {db, {db, start_link, []},
		   permanent, 5000, worker, [db]},
	{ok, {{one_for_one, 5, 30}, [Web, Db]}}.

