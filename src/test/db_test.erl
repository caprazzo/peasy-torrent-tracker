-module(db_test).

-include_lib("eunit/include/eunit.hrl").
-include("headers.hrl").
-import(db_setup).
-define(PEER_START_SEED, #peer{
		peer_key={"InfoHash","PeerId"},
		ip_port=0,
		left=0,
		last_seen=0,
		last_event=started
	}).
-define(PEER_START_LEECH, #peer{
		peer_key={"InfoHash","PeerId"},
		ip_port=0,
		left=100,
		last_seen=0,
		last_event=started
	}).

-define(M, mnesia:transaction).
peer_starts_and_is_a_seeder_and_no_torrent_test() ->
	db_setup:setup(),
	?assertEqual({atomic, ok},
				 mnesia:transaction(db:receive_announce(?PEER_START_SEED))),
	?assertEqual({atomic, [#torrent{info_hash="InfoHash", complete=1}]},
				 mnesia:transaction(fun() -> mnesia:read(torrent, "InfoHash") end)),
	?assertEqual({atomic, [?PEER_START_SEED]},
				mnesia:transaction(fun() -> mnesia:read(peer, (?PEER_START_SEED)#peer.peer_key) end)),
	db_setup:shutdown().

peer_start_and_is_a_seeder_and_update_torrent_test() ->
	db_setup:setup(),
	{aborted,azz} = mnesia:transaction(fun() ->
		mnesia:transaction(fun() -> mnesia:write(#torrent{info_hash="InfoHash", complete=1}) end),
		{atomic, oks} = ?M(db:receive_announce(?PEER_START_SEED)),
		?assertEqual({atomic, [#torrent{info_hash="InfoHash", complete=2}]},
			 mnesia:transaction(fun() -> mnesia:read(torrent, "InfoHash") end)),
		?assertEqual({atomic, [?PEER_START_SEED]},
			mnesia:transaction(fun() -> mnesia:read(peer, (?PEER_START_SEED)#peer.peer_key) end)),
		mnesia:abort(azz),
		?assertEqual({atomic, [?PEER_START_SEED]},
			mnesia:transaction(fun() -> mnesia:read(peer, (?PEER_START_SEED)#peer.peer_key) end))
	end),
	db_setup:shutdown().

peer_starts_and_is_a_leecher_and_no_torrent_testf() ->
	db_setup:setup(),
	?assertEqual({atomic, ok},
				 mnesia:transaction(db:receive_announce(?PEER_START_LEECH))),
	?assertEqual({atomic, [#torrent{info_hash="InfoHash", incomplete=1}]},
				 mnesia:transaction(fun() -> mnesia:read(torrent, "InfoHash") end)),
	?assertEqual({atomic, [?PEER_START_LEECH]},
				mnesia:transaction(fun() -> mnesia:read(peer, (?PEER_START_LEECH)#peer.peer_key) end)),
	db_setup:shutdown().

peer_start_and_is_a_leecher_and_update_torrent_testf() ->
	db_setup:setup(),
	mnesia:transaction(fun() -> mnesia:write(#torrent{info_hash="InfoHash", incomplete=5}) end),
	?assertEqual({atomic, ok},
				 mnesia:transaction(db:receive_announce(?PEER_START_LEECH))),
	?assertEqual({atomic, [#torrent{info_hash="InfoHash", incomplete=6}]},
				 mnesia:transaction(fun() -> mnesia:read(torrent, "InfoHash") end)),
	?assertEqual({atomic, [?PEER_START_LEECH]},
				mnesia:transaction(fun() -> mnesia:read(peer, (?PEER_START_LEECH)#peer.peer_key) end)),
	db_setup:shutdown().