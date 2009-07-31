-module(rest_interface).

-import(mochiweb).

-compile([export_all]).

%% @doc This module implements a REST interface for the core tracker,
%% its goal is to allow other programs (and ultimately users) to collect
%% information about the system status

%% GET /torrents/<InfoHash>
%% {info_hash=String, complete:N, incomplete:N, downloaded:N}

%% GET /torrents
%% [{info_hash=String, complete:N, incomplete:N, downloaded:N}]

%% API
%%-export([start_link/1, stop/0]).
 
%% gen_server callbacks
%%-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
%%terminate/2, code_change/3]).
 
%% testing
%%-export([handle_announce/3]).
 
start_link(Port) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).
 
init([Port]) ->
	process_flag(trap_exit, true),
	io:format("~p (~p) starting... at port ~p~n", [?MODULE, self(), Port]),
	{ok, RestRegex} = re:compile("/"),
	mochiweb_http:start([{port, Port},{name, mochiweb_rest_interface},
		{loop, fun(Req) -> dispatch_requests(Req, RestRegex) end}]),
	erlang:monitor(process, mochiweb_rest_interface),
	{ok, []}.

dispatch_requests(HttpRequest, RestRegex) ->
	{Action,_,_} = mochiweb_util:urlsplit_path(HttpRequest:get(path)),
	Parts = re:split(Action, RestRegex),
	Response = handle(Parts),
	HttpRequest:respond(Response).

handle([<<>>, <<"torrents">>, <<InfoHash>>]) ->
	{ok, Complete, Incomplete, Downloaded} = torrent_info:torrent_status(InfoHash),
	Response = {200, [{"Content-Type","text/html"}], "cazzi"},
	Response;

handle([<<>>, <<"torrents">>]) ->
	R = torrent_info:torrents_status(),
	io:format("R: ~p~n",[R]),
	Response = {200, [{"Content-Type","text/html"}], format_torrents_status([R])},
	Response;

handle(Any) ->
	{404, [{"Content-Type","text/plain"}], "404"}.
format_torrents_status([Torrents]) ->
	list_to_binary(lists:flatten(io_lib:fwrite("Torrents:~p~n", [Torrents]))).

terminate(_Reason, _State) ->
  mochiweb_http:stop(),
  ok.