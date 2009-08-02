#!/usr/bin/env escript
%%! -smp enable -sname factorial -mnesia debug verbose


main([Rootdir]) ->
	release_handler:create_RELEASES(Rootdir, Rootdir++"/releases/", Rootdir++"/releases/FIRST.rel", []).