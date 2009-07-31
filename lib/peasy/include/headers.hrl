-record(peer, {
	peer_key = {},
	last_seen = erlang:now(),
	last_event,
	uploaded = 0,
	downloaded = 0,
	left = 0,
	ip_port,
	key
}).

-record(req, {
	compact,
	no_peer_id,
	numwant=0,
	trackerid=0,
	peer = #peer{}
}).

-record(torrent, {
	info_hash,
	complete=0,		% number of peers with the entire file, i.e. seeders (integer) 
	downloaded=0,	% total number of times the tracker has registered a completion
					% ("event=complete", i.e. a client finished downloading the torrent),
	incomplete=0,	% number of non-seeder peers, aka "leechers" (integer)  
	name			% name of the torrent
}).

