-module(peasy_web_test).

-include_lib("eunit/include/eunit.hrl").

parse_req_required_test() ->
	Request = [
		{"info_hash", "infoHash"},
		{"peer_id", "peerId"},
		{"uploaded", "10000"},
		{"downloaded", "99999"},
		{"left", "1234"},
		{"port","8080"}
	],
	peasy_web:parse_req("127.0.0.1", Request).