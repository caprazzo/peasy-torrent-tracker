-module(db_setup).

-include("headers.hrl").
-export([setup/0, shutdown/0]).

setup() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	try
		mnesia:table_info(torrent,type)
	catch
		exit: _ ->
			Torrent = mnesia:create_table(torrent,[
							{type, set},
							{attributes,record_info(fields,torrent)}
				]),
			io:format("Mnesia: ~p ~p.~n",[Torrent, record_info(fields,torrent)])
	end,
	try
		mnesia:table_info(peer,type)
	catch
		exit: _ ->
			Peer = mnesia:create_table(peer,[
							{type, set},
							{attributes,record_info(fields,peer)}
				]),
			io:format("Mnesia: ~p ~p.~n",[Peer, record_info(fields,peer)])
	end,
	mnesia:wait_for_tables([torrent, peer], 5000).

shutdown() ->
	mnesia:stop().