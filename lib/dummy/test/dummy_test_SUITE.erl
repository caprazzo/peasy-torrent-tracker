-module(dummy_test_SUITE).

-compile([export_all]).
-include("ct.hrl").


all() ->
	[unittest].

unittest(Config) ->
	5 = dummy:dummy_sum(3,2).	
