-module(peasy).

%% Application root
-behaviour(application).
-import(db_setup).
-export([start/0, start/2, stop/1, stop/0]).

start() ->
	db_setup:setup(),
	application:start(?MODULE).

start(_Type, _Args) ->
	peasy_supervisor:start_link(_Args).

stop() ->
	application:stop(?MODULE).

stop(_State) ->
	ok.
	