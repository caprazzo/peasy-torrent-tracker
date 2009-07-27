-module(announce_manager).

-behviour(gen_event).

-export([init/1, handle_event/2, terminate/2]).
init(_Args) ->
	{ok, []}.

handle_event({announce, Peer}, State) ->
	io:format("Event announce/~p.~n", [Peer]),
	{ok,State}.

terminate(_Args, _State) ->
    ok.
