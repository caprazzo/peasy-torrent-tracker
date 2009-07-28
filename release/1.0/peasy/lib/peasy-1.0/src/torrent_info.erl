-module(torrent_info).

%% @doc torrent_info is a proxy gen_server to the database module.
%% provides access to torrent status and peers

-behviour(gen_server).


%% API
-export([start_link/1, shutdown/0, status/1, peers/2]).
 
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
terminate/2, code_change/3]).

start_link(Port) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

init([]) ->
  process_flag(trap_exit, true),
  io:format("~p (~p) starting...~n", [?MODULE, self()]),
  {ok, []}.

status(InfoHash) ->
	gen_server:call({local, ?MODULE}, {torrent_status, InfoHash}).

peers(InfoHash, Numwant) ->
	gen_server:call({local, ?MODULE}, {torrent_peers, InfoHash, Numwant}).

handle_call({torrent_status, InfoHash}, _From, State) ->
	db:torrent_status(InfoHash);

handle_call({torrent_peers, InfoHash, Numwant}, _From, State) ->
	db:torrent_peers(InfoHash, Numwant).

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