-module(db_test).

-include_lib("eunit/include/eunit.hrl").
-include("headers.hrl").
-import(db_setup).
-define(IP_PORT_A,"ip_port_a").
-define(PEER, #peer{
	peer_key={"InfoHash","PeerId"},
	ip_port=?IP_PORT_A,
	left=0,
	last_seen=0,
	last_event=started
}).

t(Fun) ->
	db_setup:setup(),
	db:start_link(),
	try
		Fun()
	after
		db:shutdown(),
		db_setup:shutdown()
	end.

-define(M, mnesia:transaction).
unkown_seeder_starts_test() ->
	t(fun() ->
		db:announce((?PEER)#peer{last_event=started, left=0}),
		?assertEqual({torrent_status, 1,0,0}, db:torrent_status("InfoHash")),
		?assertEqual({torrent_peers, [?IP_PORT_A]}, db:torrent_peers("InfoHash", 100))
	end).

known_seeder_starts_again_test() ->
	t(fun() ->
		db:announce((?PEER)#peer{last_event=started, left=0}),
		db:announce((?PEER)#peer{last_event=started, left=0}),
		?assertEqual({torrent_status, 2,0,0}, db:torrent_status("InfoHash")),
		?assertEqual({torrent_peers, [?IP_PORT_A]}, db:torrent_peers("InfoHash", 100))
	end).

unkown_leecher_starts_test() ->
	t(fun() ->
		db:announce((?PEER)#peer{last_event=started, left=100}),
		?assertEqual({torrent_status, 0,1,0}, db:torrent_status("InfoHash")),
		?assertEqual({torrent_peers, [?IP_PORT_A]}, db:torrent_peers("InfoHash", 100))
	end).

known_leecher_starts_again_test() ->
	t(fun() ->
		db:announce((?PEER)#peer{last_event=started, left=100}),
		db:announce((?PEER)#peer{last_event=started, left=100}),
		?assertEqual({torrent_status, 0,2,0}, db:torrent_status("InfoHash")),
		?assertEqual({torrent_peers, [?IP_PORT_A]}, db:torrent_peers("InfoHash", 100))
	end).

unknown_seeder_stops_test() ->
	t(fun() ->
		db:announce((?PEER)#peer{last_event=stopped, left=0}),
		?assertEqual({torrent_status, 0,0,0}, db:torrent_status("InfoHash")),
		?assertEqual({torrent_peers, []}, db:torrent_peers("InfoHash", 100))
	end).

known_seeder_stops_test() ->
	t(fun() ->
		db:announce((?PEER)#peer{last_event=started, left=0}),
		?assertEqual({torrent_status, 1,0,0}, db:torrent_status("InfoHash")),
		db:announce((?PEER)#peer{last_event=stopped, left=0}),
		?assertEqual({torrent_status, 0,0,0}, db:torrent_status("InfoHash")),
		?assertEqual({torrent_peers, []}, db:torrent_peers("InfoHash", 100))
	end).

unkown_seeder_completes_test() ->
	t(fun() ->
		db:announce((?PEER)#peer{last_event=completed}),
		?assertEqual({torrent_status, 1,0,1}, db:torrent_status("InfoHash")),
		?assertEqual({torrent_peers, [?IP_PORT_A]}, db:torrent_peers("InfoHash", 100))
	end).

known_leecher_completes_test() ->
	t(fun() ->
		db:announce((?PEER)#peer{last_event=started, left=1000}),	  
		db:announce((?PEER)#peer{last_event=completed}),
		?assertEqual({torrent_status, 1,0,1}, db:torrent_status("InfoHash")),
		?assertEqual({torrent_peers, [?IP_PORT_A]}, db:torrent_peers("InfoHash", 100))
	end).

unknown_seeder_announces_test() ->
	t(fun() ->
		db:announce((?PEER)#peer{last_event=undefined, left=0}),
		?assertEqual({torrent_status, 1,0,0}, db:torrent_status("InfoHash")),
		?assertEqual({torrent_peers, [?IP_PORT_A]}, db:torrent_peers("InfoHash", 100))
	end).

known_seeder_announces_test() ->
	t(fun() ->
		db:announce((?PEER)#peer{last_event=undefined, left=0}),
		db:announce((?PEER)#peer{last_event=undefined, left=0}),
		?assertEqual({torrent_status, 1,0,0}, db:torrent_status("InfoHash")),
		?assertEqual({torrent_peers, [?IP_PORT_A]}, db:torrent_peers("InfoHash", 100))
	end).

unknown_leecher_announces_test() ->
	t(fun() ->
		db:announce((?PEER)#peer{last_event=undefined, left=500}),
		?assertEqual({torrent_status, 0,1,0}, db:torrent_status("InfoHash")),
		?assertEqual({torrent_peers, [?IP_PORT_A]}, db:torrent_peers("InfoHash", 100))
	end).

known_leecher_announces_test() ->
	t(fun() ->
		db:announce((?PEER)#peer{last_event=undefined, left=500}),
		db:announce((?PEER)#peer{last_event=undefined, left=500}),
		?assertEqual({torrent_status, 0,1,0}, db:torrent_status("InfoHash")),
		?assertEqual({torrent_peers, [?IP_PORT_A]}, db:torrent_peers("InfoHash", 100))
	end).

some_unknown_leechers_start_test() ->
	t(fun() ->
		db:announce((?PEER)#peer{peer_key={"InfoHash",'A'}, last_event=started, left=500, ip_port=a}),
		db:announce((?PEER)#peer{peer_key={"InfoHash",'B'}, last_event=started, left=500, ip_port=b}),
		db:announce((?PEER)#peer{peer_key={"InfoHash",'C'}, last_event=started, left=500, ip_port=c}),
		?assertEqual({torrent_status, 0,3,0}, db:torrent_status("InfoHash")),
		{torrent_peers, Peers} = db:torrent_peers("InfoHash", 100),
		?assertEqual([a, b, c], lists:sort(Peers))
	end).

some_unknown_seeders_start_test() ->
	t(fun() ->
		db:announce((?PEER)#peer{peer_key={"InfoHash",'A'}, last_event=started, left=0, ip_port=a}),
		db:announce((?PEER)#peer{peer_key={"InfoHash",'B'}, last_event=started, left=0, ip_port=b}),
		db:announce((?PEER)#peer{peer_key={"InfoHash",'C'}, last_event=started, left=0, ip_port=c}),
		?assertEqual({torrent_status, 3,0,0}, db:torrent_status("InfoHash")),
		{torrent_peers, Peers} = db:torrent_peers("InfoHash", 100),
		?assertEqual([a, b, c], lists:sort(Peers))
	end).

