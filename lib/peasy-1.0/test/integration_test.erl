-module(integration_test).

-include_lib("eunit/include/eunit.hrl").
-include("headers.hrl").

start_stop_test() ->
	peasy:start(),
	peasy:stop().