-module(torrent_info).

%% @doc torrent_info is a proxy gen_server to the database module.
%% provides access to torrent status and peers

-behviour(gen_server).


%% API
-export([start_link/0, shutdown/0, torrent_status/1, torrents_status/0, peers/2]).
 
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
terminate/2, code_change/3]).

start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  io:format("~p (~p) starting...~n", [?MODULE, self()]),
  {ok, []}.

torrents_status() ->
	gen_server:call({global, ?MODULE}, torrents_status).

torrent_status(InfoHash) ->
	gen_server:call({global, ?MODULE}, {torrent_status, InfoHash}).

peers(InfoHash, Numwant) ->
	gen_server:call({global, ?MODULE}, {torrent_peers, InfoHash, Numwant}).

handle_call(torrents_status, _From, _State) ->
	{reply, db:torrents_status(), _State};

handle_call({torrent_status, InfoHash}, _From, _State) ->
	{reply, db:torrent_status(InfoHash), _State};

handle_call({torrent_peers, InfoHash, Numwant}, _From, _State) ->
	{reply, db:torrent_peers(InfoHash, Numwant), _State}.

handle_cast(stop, State) ->
  {stop, normal, State}.

shutdown() ->
  gen_server:cast({global, ?MODULE}, stop).
 
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  mochiweb_http:stop(),
  ok.