-module(peasy).

%% Application root
-behaviour(application).
-import(db_setup).
-export([start/0, start/2, stop/1, stop/0]).

start() ->
	%% QUESTIONS:
	%% 1: db_setup internally starts mnesia
	%% so mnesia sits outside peasy_supervisor. Is it correct?
	%% 2: is it a good idea to start mnesia hidden in db_setup?
	%% 3: is db_setup a good idea? Look good in development, but what about production?
	db_setup:setup(),
	application:start(?MODULE).

start(_Type, _Args) ->
	peasy_supervisor:start_link(_Args),
	gen_event:add_handler({local, announce_manager}, db_announce_handler, []).

stop() ->
	application:stop(?MODULE).

stop(_State) ->
	ok.
	