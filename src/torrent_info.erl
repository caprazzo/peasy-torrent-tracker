-module(torrent_info).

-behviour(gen_server).

%% API
-export([start_link/1, stop/0]).
 
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
terminate/2, code_change/3]).

start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [Port], []).

init([]) ->
  process_flag(trap_exit, true),
  io:format("~p (~p) starting...~n", [?MODULE, self()]),
  {ok, []}.

info(InfoHash) ->
	gen_server:call({global, ?MODULE}, {info, InfoHash})).

handle_call({info, InfoHash}, _From, State) ->
	db:torrent_stats(InfoHash).
  