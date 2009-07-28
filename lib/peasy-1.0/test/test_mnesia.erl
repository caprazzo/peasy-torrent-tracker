-module(test_mnesia).

-record(bottle,{id, name}).
-record(pizza,{id,name}).

-compile([export_all]).

start() ->
	mnesia:create_table(bottle,[
							{type, set},
							{attributes,record_info(fields,bottle)},
							{index,[name]}
				]).