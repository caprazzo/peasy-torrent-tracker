%%% Copyright (c) 2008 Nicolas Charpentier
%%% All rights reserved.
-module(mock).

-export([replace_module /2]).
-export([uninstall /1]).

-export([start /0]).
-export([add_module /1]).
-export([set_answer /3]).
-export([calls /0]).

replace_module (Module, Mock_module) ->
    uninstall (Module),
    {ok, Binary} = file: read_file (code: which (Mock_module)),
    File_name = atom_to_list (Module) ++ ".erl",
    code: load_binary(Module, File_name, Binary),
    ok.

uninstall (Module) ->
    code: purge (Module),
    code: delete (Module).

start () ->
    Pid = spawn_link (fun () -> mocker ([],[]) end),
    register(mocker, Pid),
    ok.

add_module (Module) ->
    Forms = forms (Module),
    uninstall (Module),
    {ok, _, Binary} = compile: forms(Forms, [report]),
    code: load_binary (Module, "foo.erl", Binary),
    ok.

set_answer (Module, Function, Answer) ->
    mocker ! {set_answer, Module, Function, Answer},
    ok.

calls () ->
    mocker ! {self(), calls},
    receive
        {calls, Calls} ->
            Calls
    end.

forms (Module) ->
    Exported_functions = find_exported_functions (Module),
    Fun = fun (F) -> function_to_form (Module, F) end,
    Functions_forms = [Fun(F) || F <- Exported_functions],
    [{attribute,1,module,Module},
     {attribute,3,export,Exported_functions}] ++
        Functions_forms  ++
        [ {function,16,wait_response,0,
           [{clause,16,[],[],
             [{'receive',20,
               [{clause,20,
                 [{tuple,20,[{atom,20,response},{atom, 20, undefined}]}],
                 [],
                 [{call,20,
                   {atom,20,throw},
                   [{atom,20,error_no_response}]}]},
                {clause,20,
                 [{tuple,20,[{atom,20,response},{var,20,'Response'}]}],
                 [],
                 [{var,19,'Response'}]}
                ]}]}]},
          {eof,23}].

function_to_form (Module, {Function, Arity}) ->
    Parameters = parameters (Arity),
    Parameters_cons = parameters_cons (Arity),
    {function,5,Function,Arity,
     [{clause,5,Parameters,[],
       [{op,6,'!',
         {atom,6,mocker},
         {tuple,6,
          [{call,6,{atom,6,self},[]},
           {atom,6,forward},
           {atom,6,Module},
           {atom,6,Function},
           Parameters_cons]}},
        {call,8,{atom,8,wait_response},[]}
       ]}]}.

parameters (0) ->
    [];
parameters (N) ->
    Seq = lists: seq (1,N),
    F = fun (I) ->
                String = lists:flatten (io_lib: format ("Var~p",[I])),
                list_to_atom(String)
        end,
    [{var, 6, F(I)} || I <- Seq].

parameters_cons (N) ->
    parameter_list_form (parameters (N)).

put_parameter_in_call (Parameters) ->
    list_to_tuple (transform_list_to_cons (Parameters)).

transform_list_to_cons ([]) ->
    [nil, 6];
transform_list_to_cons ([H|T]) ->
    [cons, 6, H, put_parameter_in_call (T)].

parameter_list_form ([]) ->
    {nil,6};
parameter_list_form (Variable_forms) ->
    put_parameter_in_call (Variable_forms).

find_exported_functions (Module) ->
    Module_info = Module: module_info (),
    All_exported = proplists: get_value (exports, Module_info),
    lists: filter (fun ({module_info,_}) ->
                           false;
                       (_) ->
                           true
                   end, All_exported).

mocker (Modules, Calls) ->
    receive
        {From, calls} ->
            From ! {calls,lists: reverse (Calls)},
            mocker (Modules, []);
        {set_answer, Module, Function, Answer} ->
            New_modules = proplists: delete ({Module, Function}, Modules),
            mocker ([{{Module, Function}, Answer}|New_modules], Calls);
        {From, forward, Module, Function, Args} ->
            Response = proplists: get_value ({Module,Function}, Modules),
            From ! {response, Response},
            mocker (Modules, [{Module, Function, Args}|Calls])
    end.
