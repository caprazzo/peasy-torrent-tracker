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
	peasy_web:handle_announce("127.0.0.1", Parms, ?MODULE).

%% Mock functions
announce(_Peer) ->
	ok.

torrent_stats(_InfoHash) ->
	{torrent_stats, 0, 0}.

torrent_peers(_InfoHash, _NumWant) ->
	{torrent_peers, []}.

