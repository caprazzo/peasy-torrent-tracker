-module(mock).
-export([await/1,await/2,signal_fun/2,new/0, expect/7, expect/6, strict/6, strict/5, o_o/5, stub/5, replay/1, verify/1, verify_after_last_call/1, verify_after_last_call/2, invocation_event/1]).

-author('Sven Heyll').
-version(2).

%% use this to create a new instance of a mock process that is in programming phase
new() ->
    Mock = spawn_link(fun() -> program_mock([],[],[]) end),
    error_logger:info_msg("mock ~w: created~n", [Mock]),
    Mock.

%% expect has the following options:
%% Orderchecking types: in_order, out_of_order, stub;  
%% Answering: {return, ...}|{error, ...}|{throw, ...}|{exit, ...}|{rec_msg, Pid}|{function, Fun(Args)} -> RetVal}|{function1, Fun(ArgList)}
expect(Mock, Type, Module, Function, Arguments, Answer = {AT, _}) when is_list(Arguments), AT==return;AT==error;AT==throw;AT==exit;AT==rec_msg;AT==function;AT==function1 ->
    call(Mock, {expect, Type, Module, Function, length(Arguments), {Arguments, Answer}}).

%% this version of expect is suited for useing custom argument matchers
expect(Mock, Type, Module, Fun, Arity, MatcherFun, Answer) when is_integer(Arity), is_function(MatcherFun)->
    call(Mock, {expect, Type, Module, Fun, Arity, {custom, MatcherFun, Answer}}).
    
%% this is a short cut for expect(.., in_order, ..)
strict(Mock, M,F,Arity, Fun, Answer) when is_integer(Arity)->
    expect(Mock, in_order, M, F, Arity, Fun, Answer).

%% this is a short cut for expect(.., in_order, ..)
strict(Mock, M,F,Args, Answer) ->
    expect(Mock, in_order, M, F, Args, Answer).

%% this is a short cut for expect(.., in_order, ..)
o_o(Mock, M,F,Args, Answer) ->
    expect(Mock, out_of_order, M, F, Args, Answer).

%% this is a short cut for expect(.., in_order, ..)
stub(Mock, M,F,Args, Answer) when is_list(Args)->
    expect(Mock, stub, M, F, Args, Answer);

%% this is a short cut for expect(.., in_order, ..)
stub(Mock, M,F,Arity, Answer) when is_integer(Arity) ->
    expect(Mock, stub, M, F, Arity, fun(_) -> true end, Answer).

%% after the programming phase call this to switch to the replay phase
replay(Mock) ->
    call(Mock, replay).

%% after the verification phase use this to verify that all expected invocations occured
verify(Mock) ->    
    verify_after_last_call(Mock, 0).

%% after the verification phase use this to verify that all expected invocations occured
verify_after_last_call(Mock) ->
    verify_after_last_call(Mock, 1500).
verify_after_last_call(Mock, TimeOut) ->
    catch(await(invocation_list_empty, TimeOut)),
    call(Mock, verify),
    await(cleanup_finished),
    error_logger:info_msg("mock ~p: verify finished~n~n~n", [Mock]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% utility functions for dealing with mock code called from a new process.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% this will create an answering function that captures the current pid and
%% sends an atom to that pid, make sure to use await(atom()) to block until that
%% message is sent 
signal_fun(Atom, RetVal) ->
    SelfP = self(),
    {function1, fun(_) -> 
   signal(SelfP, Atom),
   RetVal
  end}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
%% internal signal function that send a message in "signal" protocol format to
%% some "await(...)"
signal(Pid, Atom) ->
    error_logger:info_msg("signalling ~p from  ~p to ~p~n", [Atom, self(), Pid]),
    Pid ! {mock_signal, Atom}.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% this block the current process until signal(SameAtom) from another process is
%% invoked
await(Atom) when is_atom(Atom) ->
    await(Atom, 2000).

await(Atom,To) when is_atom(Atom)->       
    error_logger:info_msg("now awaiting ~p in process ~p~n", [Atom, self()]),
    receive
 {mock_signal, Atom} ->
     error_logger:info_msg("await succeeded: ~p~n", [Atom])
    after To ->
     fail({timeout, await, Atom})
    end.
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fail(Pid, Reason) ->
    error_logger:info_msg("mock ~w: failed: ~w~n",[self(), Reason]),
    Pid ! {error, Reason}.

fail(Reason) ->
    error_logger:info_msg("mock ~w: failed: ~w~n",[self(), Reason]),
    throw({mock_failure, Reason}).

success(Pid) ->
    Pid ! {response, ok},
    success().

success() ->
    error_logger:info_msg("mock ~w: successfully finished.~n",[self()]),
    test_passed.    

call(Name, Request) ->
    Name ! {self(), Request},
    receive
 {error, Reason} ->
     throw({mock_failure, Reason});
 {response, Response} ->
     Response
    end.

filter_fun(_, _, _, {Arguments, _}) ->    
    fun(Args) ->
     Args == Arguments
    end;

filter_fun(_, _, _, {custom, Matcher, _}) ->
    Matcher.

answer_fun({_, Answer}) ->    
    Answer;

answer_fun({custom, _, Answer}) ->
    Answer.

module_header_abstract_form(Mod) ->
    [{attribute,0,module,Mod}, 
    {attribute,0,compile,[export_all]}].

fundef_to_abstract_meta_form(Self, Mod, FunName, Arity) ->
    Line = 1,
    Params = [{var, Line, list_to_atom("A" ++ integer_to_list(I))} || I <- seq(1,Arity)],
    {function, Line, FunName, Arity,
     [{clause, Line, 
       Params, [], 
       [{call, Line, 
  {remote, Line, {atom, Line, mock}, {atom, Line, invocation_event}}, 
  [{tuple, Line, 
    [{string,Line, Self}, {atom, Line, Mod}, {atom,Line, FunName}, {integer, Line, Arity},
     lists:foldr(
       fun(E,R) ->
        {cons, Line, E, R}
       end, 
       {nil, Line}, 
       Params)]}]}]}]}.

compile_and_load_abstract_form(AbsForm) ->
    CompRes = compile:forms(AbsForm),
    {ok, Mod, Code} = CompRes,
    code:purge(Mod),
    code:delete(Mod),
    {module, _} = load_module(Mod, Code).

extract_module_set(Combined) ->
    sets:from_list(lists:map(fun([{M,_,_}|_]) -> M end, Combined)).

program_mock(InOrder, OutOfOrder, Stub) ->
    receive 
 {From, {expect, Type, Mod, Fun, Arity, Arg}} ->
     FunDef = [{Mod, Fun, Arity} | {filter_fun(Mod,Fun,Arity,Arg), answer_fun(Arg)}],
     From ! {response, ok},
     case Type of 
  in_order ->
      program_mock([FunDef | InOrder], OutOfOrder, Stub);
  out_of_order ->
      program_mock(InOrder, [FunDef | OutOfOrder], Stub);
  stub ->
      program_mock(InOrder, OutOfOrder, [FunDef | Stub])
     end;

 {From, replay}  ->
     Self = pid_to_list(self()),
      Combined = InOrder ++ OutOfOrder ++ Stub,
      ModuleSet = extract_module_set(Combined),
      sets:fold( fun (Mod, _) ->
          FunsOfModSet = sets:from_list(
      lists:foldl(
        fun([{M,F,A}|_], Acc) -> 
         if Mod == M -> [{F,A}|Acc];           
            true -> Acc
         end
        end, [], Combined)),
          HeaderForm = module_header_abstract_form(Mod),
          FunctionForms = sets:fold(
       fun({F,A},FFAcc) -> 
        [fundef_to_abstract_meta_form(Self, Mod, F, A)|FFAcc]
       end,
       [],
       FunsOfModSet),
          CLRes = compile_and_load_abstract_form(HeaderForm ++ FunctionForms),
          error_logger:info_msg("mock ~w: created and loaded mock code ~w~n",[self(),CLRes])
         end,
         [],
         ModuleSet),
       From ! {response, ok},
     %% spawn a cleanup process that will call the uninstall fun
     auto_cleanup(fun() ->
     uninstall(ModuleSet),
     signal(From, cleanup_finished)
    end),
      record_invocations(lists:reverse(InOrder), 
          OutOfOrder, 
          Stub,
          fun() -> 
           signal(From, invocation_list_empty) 
          end
         );
 {From, What} ->     
     fail(From, {invalid_state, What})
    end.
 
record_invocations([], [], Stub, EmptyFun) when is_function(EmptyFun) ->
    EmptyFun(),
    record_invocations([], [], Stub, undefined);
record_invocations(InOrder, OutOfOrder, Stub, EF) ->
    %% wait for all incoming invocations, expect every invocation and crash if the invocation was not correct
    receive
 Invocation = {ProcUnderTestPid, Mod, Fun, Arity, Args} ->
     InvMatcher = fun ([{M,F,A}|{Pred,_}]) ->
     {M,F,A} == {Mod, Fun, Arity} andalso Pred(Args)
    end,
     try 
  case InOrder of
      [Test| _] ->  InvMatcher(Test);
      [] -> false
  end
  of
  true ->       
      [[_|{_,Function}]| IOR] = InOrder,
      ProcUnderTestPid ! {mock_process_gaurd__, Function},
      record_invocations(IOR, OutOfOrder, Stub, EF);

  false -> 
      case lists:splitwith(InvMatcher, OutOfOrder) of
   {[OOODef|Rest1],Rest2} ->
       [_|{_,Function}] = OOODef,        
       ProcUnderTestPid ! {mock_process_gaurd__, Function},
       record_invocations(InOrder, Rest1 ++ Rest2, Stub, EF);

   {[], _} ->
       case lists:filter(InvMatcher, Stub) of
    [StubDef|_] ->
        [_|{_,Function}] = StubDef,        
        ProcUnderTestPid ! {mock_process_gaurd__, Function},
        record_invocations(InOrder, OutOfOrder, Stub, EF);

    _ ->
        EF(),
        Reason = {unexpected_invocation, Invocation},
        ProcUnderTestPid ! {mock_process_gaurd__, {error, Reason}},
        fail(Reason)
       end
      end
     catch
  ET:EX -> 
      Reason = {matching_function_is_incorrent, Invocation, {ET, EX}},
      ProcUnderTestPid ! {mock_process_gaurd__, {error, Reason}},
      EF(),
      fail(Reason)
     end; 

 {From, verify} ->
     case {InOrder,OutOfOrder} of
  {[],[]} -> 
      success(From);
  MissingRest ->
      fail(From,{expected_invocations_missing, MissingRest})
     end;

 {From, What} ->
     EF(),
     fail(From, {invalid_state, What})

    end.

invocation_event({MockPidStr, Mod, Fun, Arity, Args}) ->
    MockPid = list_to_pid(MockPidStr),
    error_logger:info_msg("mock ~w: invocation: ~w:~w/~w ~w~n",[MockPid, Mod, Fun, Arity, Args]),
    MockPid ! {self(), Mod, Fun, Arity, Args},
    receive
 {mock_process_gaurd__, {return, Answer}} -> 
     Answer;
 {mock_process_gaurd__, {error, E}} ->
     erlang:error(E);
 {mock_process_gaurd__, {throw, E}} ->
     throw(E);
 {mock_process_gaurd__, {exit, R}} ->
     exit(R);
 {mock_process_gaurd__, {function, F}} ->
     error_logger:info_msg("mock ~w: invoking answerer~n",[MockPid]),     
     R = apply(F,Args),
     error_logger:info_msg("mock ~w: answerer returned: ~w~n",[MockPid,R]),
     R;
 {mock_process_gaurd__, {function1, F}} ->
     error_logger:info_msg("mock ~w: invoking answerer~n",[MockPid]),     
     R = F(Args),
     error_logger:info_msg("mock ~w: answerer returned: ~w~n",[MockPid,R]),
     R;
 {mock_process_gaurd__, {rec_msg, P}} ->
     error_logger:info_msg("mock ~w: receiving message for ~w~n",[MockPid,P]),     
     Msg = receive 
        M ->
     P ! M
    end,
     error_logger:info_msg("mock ~w: message ~w delivered to ~w~n",[MockPid,Msg,P])    
    end.

seq(A, E) when A > E -> [];
seq(A, E) -> lists:seq(A,E).
    
uninstall(ModuleSet) ->
    lists:map(fun(Mod) ->
        error_logger:info_msg("Deleting and purging module ~p~n", [Mod]),
        code:purge(Mod),
        code:delete(Mod)
       end, sets:to_list(ModuleSet)).

auto_cleanup(CleanupFun) ->    
    spawn_link(fun() ->
         erlang:process_flag(trap_exit, true),
         error_logger:info_msg("auto cleanup handler ~p waiting for the end...~n", [self()]),
         receive
      Msg = {'EXIT', _From, _Reason} ->
          error_logger:info_msg("auto cleanup handler ~p receive exit message ~p.~n", [self(), Msg]),
          CleanupFun();
      _Ather -> 
          error_logger:info_msg("auto cleanup handler ~p received unexpected message  ~p.~n", [self(), _Ather])
         end
        end).          
