-module(log_announce_handler).

-behviour(gen_event).
-include("headers.hrl").
-export([init/1, handle_event/2, terminate/2]).
init(_Args) ->
	process_flag(trap_exit, true),
	io:format("~p (~p) starting...~n", [?MODULE, self()]),
	{ok, []}.

handle_event({announce, Peer}, State) ->
	{H,I} = Peer#peer.peer_key,
	log4erl:info("Announce received H:~p I:~p IP:~p E:~p U:~p D:~p L:~p.~n ",
				 [hex:bin_to_hexstr(H),
				  hex:bin_to_hexstr(I),
				  hex:bin_to_hexstr(Peer#peer.ip_port),
				  Peer#peer.last_event,
				  Peer#peer.uploaded,
				  Peer#peer.downloaded,
				  Peer#peer.left]),
	{ok, State}.

terminate(_Args, _State) ->
    ok.