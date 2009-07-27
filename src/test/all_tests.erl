-module(all_tests).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
	[{module, peasy_web_test},
	 {module, db_test}].