-module(db).

%% @doc Database create, update and delete routines for #peer and #torrent related
%% to the "announce" event. This also loads torrent stats and peers

%% By design registration of peers or torrents is not required, and an effort is made
%% in accepting unexpected transactions such as "a peer that at his first announce completes
%% a torrent not in the database".

%% To do this, peer records are overwritten or deleted in mont cases (when the peer
%% is announcing with no event, the last_event field is not overwritten; see last clause).
%% Equally, torrent records referenced by the peer are updated or created if absent.

-behaviour(gen_server).
-import(db_setup).
%% exported for unit tesintg
-export([receive_announce/1]).
-include("headers.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([start_link/0, shutdown/0]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2,
	 terminate/2, code_change/3]).

-export([announce/1, torrent_peers/2, torrent_stats/1, dump/0]).

start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
	process_flag(trap_exit, true),
	io:format("~p (~p) starting...~n", [?MODULE, self()]),
	{ok, dict:new()}.

shutdown() ->
  gen_server:cast({global, ?MODULE}, stop).

%% A peer is announcing his new state.
%% This operation is asynchronous, gen_server:cast returns immediately
announce(Peer) ->
	gen_server:cast({global, ?MODULE},{announce,Peer}).

torrent_peers(InfoHash, Num) ->
	gen_server:call({global, ?MODULE}, {peer_list, InfoHash, Num}).

torrent_stats(InfoHash) ->
	gen_server:call({global, ?MODULE}, {stats, InfoHash}).

%% Handle announce calls asynchronously.
handle_cast({announce, Peer}, _State) ->
	F = receive_announce(Peer),
	mnesia:transaction(F),
	{noreply, _State};
handle_cast(stop, State) ->
  {stop, normal, State}.

handle_call({stats, InfoHash}, _From, _State) ->
	F = fun() -> mnesia:read({torrent, InfoHash}) end,
	case mnesia:transaction(F) of
		{atomic, []} ->
			{reply, {torrent_stats,0,0}, _State};
		
		{atomic,[#torrent{complete=Complete, incomplete=Incomplete}]} ->
			{reply, {torrent_stats, Complete, Incomplete}, _State}
	end;
handle_call({peer_list, InfoHash, _Num}, _From, _State) ->
	Q = qlc:q([IpPort || #peer{peer_key={Hash,_},ip_port=IpPort}<-mnesia:table(peer), Hash=:=InfoHash]),
	F = fun() -> qlc:e(Q) end,
	case mnesia:transaction(F) of
		{atomic, Peers} ->
			{reply, {torrent_peers, Peers}, _State}
	end.
	

%% This group of receive_announce uses header pattern matching to determine what kind of event
%% is being announced and what is the state of the peer relatively to one torrent (record #torr).

%% Peer starts and is a seeder.
%% update: complete+1
%% create: complete=1
receive_announce(#peer{last_event=started,left=0, peer_key={InfoHash, _PeerId}}=Peer) ->
	fun() ->
		case mnesia:read({torrent, InfoHash}) of
			[] -> mnesia:write(#torrent{complete=1, info_hash = InfoHash});		  
			[#torrent{complete=Complete}=Torrent] ->
				mnesia:write(Torrent#torrent{complete=Complete+1})
		end,
		mnesia:write(Peer)
	end;

%% Peer starts and is a leecher.
%% update: incomplete+1 
%% create: incomplete=1
%% create or update peer
receive_announce(#peer{last_event=started, peer_key={InfoHash, _PeerId}}=Peer) ->
	fun() ->
		case mnesia:read({torrent, InfoHash}) of
			[] -> mnesia:write(#torrent{incomplete=1, info_hash = InfoHash});
			[#torrent{incomplete=Incomplete}=Torrent] ->
				mnesia:write(Torrent#torrent{incomplete=Incomplete+1})
		end,
		mnesia:write(Peer)
	end;

%% Peer is stopping and is a seeder.
%% update: complete-1
%% create: empty
%% delete peer
receive_announce(#peer{last_event=stopped, left=0, peer_key={InfoHash, _PeerId}}=Peer) ->
	fun() ->
		case mnesia:read({torrent, InfoHash}) of
			[] -> mnesia:write(#torrent{info_hash = InfoHash});
			[#torrent{complete=Complete}=Torrent] ->
				mnesia:write(Torrent#torrent{complete=Complete-1})
		end,
		mnesia:delete(Peer)
	end;

%% Peer is stopping and is a leecher.
%% update: incomplete-1
%% create: empty
%% delete peer
receive_announce(#peer{last_event=stopped, peer_key={InfoHash, _PeerId}}=Peer) ->
	fun() ->
		case mnesia:read({torrent, InfoHash}) of
			[] -> mnesia:write(#torrent{info_hash = InfoHash});
			[#torrent{incomplete=Incomplete}=Torrent] ->
				mnesia:write(Torrent#torrent{incomplete=Incomplete-1})
		end,
		mnesia:delete(Peer)
	end;

%% Peer completed download.
%% update: complete+1, incomplete-1, download+1
%% create: complete=1
%% create or update peer
receive_announce(#peer{last_event=completed, peer_key={InfoHash, _PeerId}}=Peer) ->
	fun() ->
		case mnesia:read({torrent, InfoHash}) of
			[] -> mnesia:write(#torrent{complete=1, info_hash = InfoHash});
			[#torrent{complete=Complete, incomplete=Incomplete, downloaded=Downloaded}=Torrent] ->
				mnesia:write(Torrent#torrent{
					complete=Complete+1,
					incomplete=Incomplete-1,
					downloaded=Downloaded+1})
		end,
		mnesia:write(Peer)
	end;
		   
%% Peer is just announcing.
%% create or update the peer (if update, don't touch last_event)
receive_announce(Peer) ->
	fun() ->
		case mnesia:read({peer, Peer#peer.peer_key}) of
			[] -> mnesia:write(Peer);
			[#peer{last_event=Event}] ->
				mnesia:write(Peer#peer{
					last_event=Event
				})
		end
	end.


dump() ->
	Qp = qlc:q([ Peer || Peer <- mnesia:table(peer)]),
	{atomic, Peers} = mnesia:transaction(fun() -> qlc:e(Qp) end),
	Qt = qlc:q([ X || #torrent{info_hash=X} <- mnesia:table(torrent)]),
	{atomic, Torrents} = mnesia:transaction(fun() -> qlc:e(Qt) end),
	print_torrent(Torrents),
	print_peer(Peers).

print_torrent([]) ->
	nil;
print_torrent([#torrent{info_hash=Hash,incomplete=Incomplete, complete=Complete, downloaded=Downloaded}|Torrents]) ->
	io:format("[Torr ~s ~p ~p ~p]",[Hash,Incomplete,Complete,Downloaded]),
	print_torrent(Torrents).

print_peer([]) ->
	nil;
print_peer([#peer{peer_key={Hash,Id},ip_port=Addr,last_event=Event}|Peers]) ->
	io:format("[Peer ~s ~s ~s ~s]~n",[hex:bin_to_hexstr(list_to_binary(Hash)), Id, Addr, Event]),
	io:format("Peer Hash=~s Id=~s Addr=~s Event=~s ~n", [Hash, Id, Addr, Event]),
	print_peer(Peers);
print_peer(Any) ->
	io:format("Any: ~p.~n",[Any]).


handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.