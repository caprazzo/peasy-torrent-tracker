#!/usr/bin/env escript
%%! -smp enable -sname factorial -mnesia debug verbose


main(_) ->
	systools:make_script("FIRST", [no_module_tests, {path, ["../lib/*/ebin"]}]),
	systools:make_tar("FIRST", [no_module_tests, {path, ["../lib/*/ebin"]}, {dirs, [include, src]}, {erts, code:root_dir()}]),
	io:format(code:root_dir()).
	
