-module(db_announce_handler).

-behviour(gen_event).

-export([init/1, handle_event/2, terminate/2]).
init(_Args) ->
	{ok, []}.

handle_event({announce, Peer}, State) ->
	io:format("Db Event announce/~p.~n", [Peer]),
	db:announce(Peer),
	{ok, State}.

terminate(_Args, _State) ->
    ok.