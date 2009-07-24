-module(loadtest).
-export([start/0,gen_hash/2, generator_loop/4, timer/2, start_stress/0]).
-define(UPLOAD_LIMIT, 10000).
-define(TORRENT_SIZE, 10000).
-define(NO_EVT, "").

-record(dl, {hash,key,up=0,down=0,left=?TORRENT_SIZE,evt=started,leech=true}).
start() ->
	inets:start(),
	Torrents = gen_many_hashes(100, []),
	Self = self(),
	io:format("Starting stress test.~n"),
	spawn_link(?MODULE, timer, [1000, fun() -> Self ! {stats} end]),
	generator_loop(Torrents, Self),
	generator_loop(Torrents, Self),
	generator_loop(Torrents, Self),
	receive_loop({0,0,0},{0,0,0}).

start_stress() ->
	inets:start(),
	Self = self(),
	io:format("Starting stress test.~n"),
	spawn_link(?MODULE, timer, [1000, fun() -> Self ! {stats} end]),
	Url = gen_url(#dl{hash=gen_hash(20,[]),key=gen_hash(20,[])}),
	stress_loop(Url, {0,0,0}, {0,0,0}).

stress_loop(Url, {XActive, _, _}=HttpStats, {_OActive, OClosed, _OErro}=OldStats) ->
	case XActive of
		0 ->
			
			http:request(get, {Url, []}, [{version, "HTTP/1.0"}], [{sync, false},  {stream, self}]);
		_Any -> noop
	end,
	{Active, Closed, Error} = receive
        {stats} -> io:format("http: ~w\n",[HttpStats]),
				   {XActive,0,0}
    	after 0 -> 
			HttpStats
	end,
	receive
		{http,{_Ref,stream_start,_Rt}} ->
			stress_loop(Url, {Active+1, Closed, Error}, OldStats);
		{http,{_Ref,stream_end,_Rt}} ->
			%io:format("Rt: ~p~n", [Rt]),
			stress_loop(Url, {Active-1, Closed+1, Error}, OldStats);
            
		{http,{_Ref,{error,_Why}}} ->
			io:format("Error: ~w.~n",[_Why]),	
			stress_loop(Url, {Active-1, Closed+1, Error+1}, OldStats)
	end.

xseq(N) ->
	xseq(N,[]).
xseq(0, Acc) ->
	Acc;
xseq(N, Acc) ->
	xseq(N-1,[x|Acc]).

receive_loop({Active, Closed, Error} = HttpStats, {Seeders, Leechers, Completed}=TorrStats) ->
	%% this receive won't wait (will wait 0) for messagges, it
	%% will only process messages alredy in the inbox.
	%case Active of 
	%	0 -> Gen ! new_client;
	%	_Any -> noop
	%end,			 
	receive
        {stats} -> io:format("http: ~w ~w\n",[HttpStats, TorrStats])
    	after 0 -> noop
	end,
	receive
		{http,{_Ref,stream_start,_Rt}} ->
			receive_loop({Active+1, Closed, Error}, TorrStats);
		{http,{_Ref,stream_end,_Rt}} ->
			%io:format("Rt: ~p~n", [Rt]),
			receive_loop({Active-1, Closed+1, Error}, TorrStats);
            
		{http,{_Ref,{error,_Why}}} ->
			io:format("Error: ~w.~n",[_Why]),	
			receive_loop({Active-1, Closed+1, Error+1}, TorrStats);
			
		{create_request, Url} ->
			receive
			after 10 -> noop
			end,
			http:request(get, {Url, []}, [{version, "HTTP/1.0"}], [{sync, false},  {stream, self}]),
			receive_loop(HttpStats, TorrStats);
		{torr_stats, {S, L, C}} ->
			receive_loop(HttpStats, {Seeders+S, Leechers+L, Completed+C});
	
		Msg ->
			%io:format("Other: ~p~n",[Msg]),
			receive_loop(HttpStats, TorrStats)
	end.


%% Download is empty, create a new one and put it at the bottom of the stack

%% Generator loop works this way:
%% each clause of generator_loop picks an element from Downloads
%% and inserts it at the bottom with updated statistics; each download has a lifecycle
%% Dummy -> Started -> Leecher -> Complete -> Seeder -> Stopped -> Dummy
%% A download will stay Leecher until left=0, then seeder until Up=?UPLOAD_LIMIT
%% dummy -> started

%% pick a dummy element from the head, pick one torrent from the list of torrents
%% create a proper download and shove it at bottom of the stack. 

%% Dummy -> Started
generator_loop(Torrents, Parent) ->
	spawn_link(?MODULE, generator_loop,
		  [Torrents, xseq(1000),
			fun(Download) -> Parent ! {create_request, gen_url(Download)}, receive after 10 -> noop end end,
			fun(TorrStats) -> Parent ! {torr_stats, TorrStats} end
		   ]).

generator_loop([T|Torrents], [x|Dls], Fun, F) ->
	PeerId = gen_hash(20, []),
	Dl = #dl{hash=T, key=PeerId, evt=started},
	
	log("x -> ~p~n", [Dl]),
	generator_loop(Torrents++[T], [Dl|Dls],Fun,F);

%% Started -> Leecher
generator_loop(T, [#dl{evt=started}=Dl|Dls], Fun,F) ->
	Fun(Dl),
	F({0, 1, 0}), % +1 leecher
	UpdatedDl = Dl#dl{evt=?NO_EVT},
	log("started->leecher ~p -> ~p~n", [Dl, UpdatedDl]),
	generator_loop(T, Dls ++ [UpdatedDl], Fun,F);

%% Leecher -> Leecher
generator_loop(T, [#dl{up=U, down=D, left=L, evt=?NO_EVT}=Dl|Dls], Fun, F)  when L > 0 ->
	Fun(Dl),
	UpdatedDl = Dl#dl{up=U+50, down=D+400, left=L-400},
	log("leecher->leecher ~p -> ~p~n", [Dl, UpdatedDl]),
	generator_loop(T, Dls ++ [UpdatedDl], Fun, F);

%% Leecher -> Completed
generator_loop(T, [#dl{left=L, evt=E, leech=true}=Dl|Dls], Fun,F) when L =< 0, E=:=?NO_EVT ->
	Fun(Dl),
	F({1,-1,1}), %% +1 seeder, -1 leechers,  +1 completed
	UpdatedDl = Dl#dl{evt=completed, left=L, leech=false},
	log("leecher->completed ~p -> ~p~n", [Dl, UpdatedDl]),
	generator_loop(T, Dls ++ [UpdatedDl], Fun,F);

%% Completed -> Seeder 
generator_loop(T, [#dl{evt=E}=Dl|Dls], Fun, F) when E=:=completed ->
	Fun(Dl),
	UpdatedDl = Dl#dl{evt=?NO_EVT},
	log("completed->seeder ~p -> ~p~n", [Dl, UpdatedDl]),
	generator_loop(T, Dls ++ [UpdatedDl], Fun, F);

%% Seeder -> Seeder
generator_loop(T, [#dl{up=U, left=0, evt=?NO_EVT}=Dl|Dls], Fun, F) when U < ?UPLOAD_LIMIT ->
	Fun(Dl),
	UpdatedDl = Dl#dl{up=U+50},
	log("seeder->seeder ~p -> ~p~n", [Dl, UpdatedDl]),
	generator_loop(T, Dls ++ [UpdatedDl], Fun, F);

%% Seeder -> Stopped
generator_loop(T, [#dl{up=U, evt=?NO_EVT}=Dl|Dls], Fun,F) when U >= ?UPLOAD_LIMIT ->
	Fun(Dl),
	F({-1,0,0}), 	%% -1 seeder
	UpdatedDl = Dl#dl{evt=stopped},
	log("seeder->stopped ~p -> ~p~n", [Dl, UpdatedDl]),
	generator_loop(T, Dls ++ [UpdatedDl], Fun,F);

%% Stopped -> Dummy
generator_loop(T, [#dl{evt=stopped}=Dl|Dls], Fun, F) ->
	Fun(Dl),
	generator_loop(T, Dls ++ [x], Fun, F).

log(_Format, _Values) ->
	%io:format(_Format, _Values).
	nil.

gen_url(#dl{hash=Hash,key=Peer,up=Up,down=Down,left=Left,evt=Event}) ->
	lists:flatten(io_lib:fwrite("http://localhost:8080/announce?info_hash=~s&peer_id=~s&port=1234&uploaded=~p&downloaded=~p&left=~p&event=~s&ip=192.168.0.1",
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
	
			   