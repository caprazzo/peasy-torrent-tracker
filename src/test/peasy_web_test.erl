-module(peasy_web_test).

-include_lib("eunit/include/eunit.hrl").
-include("headers.hrl").

-export([announce/1, torrent_stats/1, torrent_peers/2]).

accept_announce_started_test() ->
	Parms = [
		{"info_hash", "INFOHASH"},
		{"peer_id", "PEERID"},
		{"port", "9090"},
		{"event", "started"},
		{"uploaded","0"},
		{"downloaded","0"},
		{"left","10000"}
	],
	%% ?MODULE is added at the end so that this module is executed instead of 'db' 
	Response = peasy_web:handle_announce("127.0.0.1", Parms, {33, 44, ?MODULE}),
	io:format("~p~n",[Response]),
	?assertEqual({200, [{"Content-Type","text/plain"},{"Content-Length",74}],<<"d8:completei0e10:incompletei0e8:intervali33e12:min intervali441e5:peers0:e">>}, Response).

%% Mock functions
announce(_Peer) ->
	ok.

torrent_stats(_InfoHash) ->
	{torrent_stats, 0, 0}.

torrent_peers(_InfoHash, _NumWant) ->
	{torrent_peers, []}.

