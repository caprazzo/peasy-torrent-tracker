-module(loadtest).
-export([start/0,gen_hash/2, generator_loop/4, timer/2]).
-define(UPLOAD_LIMIT, 100000).
-define(TORRENT_SIZE, 100000).

start() ->
	inets:start(),
	Torrents = gen_many_hashes(100, []),
	Clients = gen_many_hashes(100, []),
	Self = self(),
	io:format("Starting stress test.~n"),
	spawn_link(?MODULE, timer, [1000, fun() -> Self ! {stats} end]),
	spawn_link(?MODULE, generator_loop,
		  [Torrents, Clients, [x,x,x,x,x,x,x,x,x,x],
			fun(Url) -> Self ! {create_request, Url}, receive after 100 -> noop end end]),
	receive_loop({0,0,0}).

%% Download is empty, create a new one and put it at the bottom of the stack

%% Generator loop works this way:
%% each clause of generator_loop picks an element from Downloads
%% and inserts it at the bottom with updated statistics; each download has a lifecycle
%% dummy -> started -> none ("") until left =0 -> completed -> stopped

%% dummy -> started

%% pick a dummy element from the head, pick one torrent and one hash,
%% create a proper download and shove it at bottom of the stack
%% Note that the consumed Hash is appended to Clients and the opposite; this 
%% shuffling is to reduce the probability of a collision.
generator_loop([Hash|Torrents], [Client|Clients], [x|Downloads], Fun) ->
	generator_loop(Torrents++[Client],
				   Clients++[Hash],
				   Downloads ++ [{download, Hash,Client,0,0,?TORRENT_SIZE,started}],
				   Fun);
%% Started started -> none ("")
generator_loop(Torrents, Clients, [{Hash, Client, Up, Down, Left, started}=Download|Downloads], Fun) ->
	Fun(gen_url(Download)),
	generator_loop(Torrents, Clients, Downloads ++ [{Hash, Client, Up+50, Down+400, Left-400, ""}], Fun);

%% Leecher -> Completed when event = "" and Left == 0 none -> completed 
generator_loop(Torrents, Clients, [{Hash, Client, Up, Down, 0, ""}=Download|Downloads], Fun) ->
	Fun(gen_url(Download)),
	generator_loop(Torrents, Clients, Downloads ++ [{Hash, Client, Up+50, Down, 0, completed}], Fun);

%% Seeder -> Stopped
generator_loop(Torrents, Clients, [{Hash, Client, ?UPLOAD_LIMIT=Up, Down, Left, ""}=Download|Downloads], Fun) ->
	Fun(gen_url(Download)),
	generator_loop(Torrents, Clients, Downloads ++ [{Hash, Client, Up, Down, Left, stopped}], Fun);

%% Leecher->Leecher none -> none
generator_loop(Torrents, Clients, [{Hash, Client, Up, Down, Left, ""}=Download|Downloads], Fun) ->
	Fun(gen_url(Download)),
	generator_loop(Torrents, Clients, Downloads ++ [{Hash, Client, Up+50, Down+400, Left-100, ""}], Fun);

%% completed -> stopped
generator_loop(Torrents, Clients, [{Hash, Client, Up, Down, Left, completed}=Download|Downloads], Fun) ->
	Fun(gen_url(Download)),
	generator_loop(Torrents, Clients, Downloads ++ [{Hash, Client, Up+100, Down, Left, ""}], Fun);

%% stopped -> forget client and create new dummy
generator_loop(Torrents, Clients, [Download|Downloads], Fun) ->
	Fun(gen_url(Download)),
	generator_loop(Torrents, Clients, [x|Downloads], Fun).

receive_loop({Active, Closed, Error} = Stats) ->
	%% this receive won't wait (will wait 0) for messagges, it
	%% will process messages alredy in the inbox.
	receive
        {stats} -> io:format("Stats: ~w\n",[Stats])
    	after 0 -> noop
	end,
	receive
		{http,{_Ref,stream_end,_}} -> 
			receive_loop({Active-1, Closed+1, Error});
            
		{http,{_Ref,{error,Why}}} ->
			receive_loop({Active-1, Closed+1, Error+1});
			
		{create_request, Url} ->
			http:request(get, {Url, []}, [], [{sync, false}, {stream, self}, {version, 1.1}, {body_format, binary}]),
			receive_loop({Active+1, Closed, Error})
	end.

gen_url({download, Hash,Peer,Up,Down,Left,Event}) ->
	lists:flatten(io_lib:fwrite("http://localhost:8080/announce?info_hash=~s&peer_id=~s&port=1234p&uploaded=~p&downloaded=~p&left=~p&event=~s&ip=192.168.0.1s",
				  [Hash,Peer,Up,Down,Left,Event])).

timer(T, Fun) ->
	receive
	after T ->
		Fun()
	end,
	timer(T, Fun).
%% generates a list of N ashes
gen_many_hashes(0,Acc) -> Acc;
gen_many_hashes(N,Acc) ->
	gen_many_hashes(N-1,[gen_hash(20,[])|Acc]).
	
%% generates a random sequence
%% of N uppercase characters
gen_hash(0, Acc) -> Acc;
gen_hash(N, Acc) ->
	gen_hash(N-1, [ random:uniform(25)+64|Acc]).
	
			   