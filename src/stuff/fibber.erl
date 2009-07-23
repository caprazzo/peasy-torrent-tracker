-module(fibber).

-export([calc/1, calc/2, server_calc/2, start/1, stop/0]).

start(0) ->
	ok;

start(Workers) ->
	Name = list_to_atom("worker" ++ integer_to_list(Workers)),
	pool:start(Name),
	io:format("Started ~p~n", [Name]),
	start(Workers - 1).

stop() ->
	pool:stop().

calc(N) ->
	fibber:calc(N, 10000).

calc(N, Timeout) ->
	pool:pspawn(fibber, server_calc, [self(), N]),
	receive
		{reply, Result} ->
			Result;
		Wtf ->
			io:format("Didn't expect ~p~n", [Wtf])
	after Timeout ->
		timeout
	end.

server_calc(Caller, N) ->
	Result = fib(N),
	Caller ! {reply, {node(), Result}}.

fib(1) -> 1;
fib(2) -> 1;
fib(N) when N > 2 -> fib1(N,1,1).

fib1(3,P1,P2) -> P1 + P2;
fib1(N,P1,P2) ->
    fib1(N-1,P2, P1 + P2).
