-module(dets_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").

-compile(export_all).
-compile({parse_transform, eqc_cover}).

-ifndef(BUG_OPEN_FILE).
-define(BUG_OPEN_FILE,true).
-endif.

-ifndef(BUG_INSERT_NEW).
-define(BUG_INSERT_NEW,true).
-endif.

-record(state,{name,type=bag,contents=[]}).

-generate_examples(prop_dets).

%% Definition of the states. Each state is represented by a function, 
%% listing the transitions from that state, together with generators 
%% for the calls to make each transition.
init_state(_S) ->
    [ {opened,{call,?MODULE,open_file,[dets_table,[{type,oneof([set,bag])}]]}}
     ].

opened(S) ->
    [ {init_state,{call,dets,close,[dets_table]}},
      {opened,{call,?MODULE,open_file,[dets_table,[{type,S#state.type}]]}},
      {opened,{call,dets,lookup,[dets_table,key()]}},
      {opened,{call,dets,insert,[dets_table,oneof([object(),list_or_value(object())])]}},
      {opened,{call,dets,insert_new,[dets_table,oneof([object(),list_or_value(object())])]}},
      {opened,{call,dets,delete,[dets_table,nat()]}}%,
%      {opened,{call,?MODULE,get_contents,[dets_table]}}
     ].

list_or_value(Gen) ->
    ?LET(Xs, Gen,
         case Xs of
             [X] -> X;
             _ -> Xs
         end).

object() ->
    {key(),nat()}.

key() ->
    choose(1, 3).

%% Identify the initial state
initial_state() ->
    init_state.

%% Initialize the state data
initial_state_data() ->
    #state{}.

%% Next state transformation for state data.
%% S is the current state, From and To are state names
next_state_data(_From,_To,S,V,{call,_,open_file,[_,[{type,T}]]}) ->
    S#state{name=V,type=T};
next_state_data(_From,_To,S,_V,{call,_,insert,[_,Objs]}) ->
    S#state{contents=model_insert(S#state.type,S#state.contents,Objs)};
next_state_data(_From,_To,S,_V,{call,_,insert_new,[_,Objs]}) ->
    case any_exist(Objs,S#state.contents) of
	true ->
	    S;
	false ->
	    S#state{contents=model_insert(S#state.type,S#state.contents,Objs)}
    end;
next_state_data(_From,_To,S,_V,{call,_,delete,[_,K]}) ->
    S#state{contents=model_delete(S#state.contents,K)};
next_state_data(_From,_To,S,_V,{call,_,_,_}) ->
    S.

%% Precondition (for state data).
%% Precondition is checked before command is added to the command sequence
precondition(From,_To,S,{call,_,open_file,[_,[{type,T}]]}) ->
    lists:member(S#state.type,[undefined,T])
	andalso (From==init_state orelse ?BUG_OPEN_FILE);
precondition(_From,_To,_S,{call,_,insert_new,_}) ->
    ?BUG_INSERT_NEW;
precondition(_From,_To,_S,{call,_,_,_}) ->
    true.

%% Postcondition, checked after command has been evaluated
%% OBS: S is the state before next_state_data(From,To,S,_,<command>) 
postcondition(_From,_To,_S,{call,_,close,_},Res) ->
    Res==ok orelse Res=={error,not_owner};  %% not_owner in parallel test;
postcondition(_From,_To,S,{call,_,lookup,[_,Key]},Res) ->
    case length(Res) of
        0 -> ok;
        1 -> ok;
        _ -> ok
    end,
    lists:sort(Res) == lists:sort([O || O <- S#state.contents,
					element(1,O) == Key]);
postcondition(_From,_To,_S,{call,_,delete,_},Res) ->
    Res==ok;
postcondition(_From,_To,_S,{call,_,insert,_},Res) ->
    Res==ok;
postcondition(_From,_To,S,{call,_,insert_new,[_,Objs]},Res) ->
    Res==not any_exist(Objs,S#state.contents);
postcondition(_From,_To,S,{call,_,get_contents,_},Res) ->
    lists:sort(Res) == lists:sort(S#state.contents);
postcondition(_From,_To,_S,{call,_,_,_},_Res) ->
    true.

%% Model

model_insert(set,S,{K,_V}=Obj) ->
    lists:keydelete(K,1,S)++[Obj];
model_insert(bag,S,{_K,_V}=Obj) ->
    (S--[Obj])++[Obj];   % surprise! why can't Obj appear twice?
model_insert(T,S,[Obj|Objs]) ->
    model_insert(T,model_insert(T,S,Obj),Objs);
model_insert(_,S,[]) ->
    S.

any_exist(Obj,S) when is_tuple(Obj) ->
    any_exist([Obj],S);
any_exist(Objs,S) ->
    [K || {K,_} <- Objs,
	  lists:keymember(K,1,S)]
	/= [].

model_delete(S,K) ->
    [O || O={K1,_} <- S,
	  K1/=K].

%% Top level property

prop_dets() ->
  ?FORALL(Cmds, more_commands(3, commands(?MODULE)),
	    ?TRAPEXIT(
	       begin
		   dets:close(dets_table),
		   file_delete(dets_table),
		   {H,_S,Res} = run_commands(?MODULE,Cmds),
       Negative = [false],
       find_examples:generate_examples(?MODULE, Cmds, H, Res, Negative,
                                       aggregate(command_names(Cmds),
                                                 Res == ok))
         end)
         ).

prop_cover() ->
  ?FORALL(Cmds, ex_cover:gen_commands(?MODULE),
          ?TRAPEXIT(
             begin
               dets:close(dets_table),
               file_delete(dets_table),
               ex_cover:ex_coverage(?MODULE, Cmds, fun(_H, _S, Res) ->
                 aggregate(command_names(Cmds),
                           Res == ok) end)
             end)
         ).

prop_swap() ->
  ?FORALL(SwapCmds, ex_swap:gen_swapcommands(?MODULE),
          ?TRAPEXIT(
            begin
              Cmds = ex_swap:get_commands(SwapCmds),
              dets:close(dets_table),
              file_delete(dets_table),
              {H, _S, Res} = run_commands(?MODULE, Cmds),
              ex_swap:interesting(?MODULE, SwapCmds, H, Res)
            end)
         ).

prop_param() ->
  ?FORALL(Cmds, find_examples:gen_commands_fsm(?MODULE),
	    ?TRAPEXIT(
	       begin
           dets:close(dets_table),
           file_delete(dets_table),
           {H,_S,Res} = eqc_fsm:run_commands(?MODULE,Cmds),
           Negative = [false],
           find_examples:generate_examples(?MODULE, Cmds, H, Res, Negative,
                                           aggregate(command_names(Cmds),
                                                     Res == ok))
         end)
         ).
%		   Corrupt = Cmds/=[] andalso corrupted(dets_table,Type),
%		   ?WHENFAIL(
%		      io:format("History: ~p\nState: ~p\nRes: ~p\n",[H,S,Res]),
%		      aggregate(command_names(Cmds),
%				Res == ok andalso not Corrupt))
%	       end)).

%% Parallel testing property

prop_parallel() ->
    ?FORALL(Type,oneof([set,bag]),
    %% Number of attempts to make each test case fail. When searching
    %% for a failing example, we run each test once. When searching for
    %% a way to shrink a test case, we run each candidate shrinking
    %% 100 times.
    ?FORALL(Attempts,?SHRINK(1,[1000]),
      ?FORALL({Seq,Par},
	      parallel_commands(?MODULE,{init_state,#state{type=Type}}),
	?ALWAYS(Attempts,
	  ?TIMEOUT(1500,
	    begin	
		%% Make sure the table is not left open by a previous test.
		[dets:close(dets_table) || _ <- "abcdefghijkl"],
		%% Make sure the table is initially empty.
		file_delete(dets_table),
		%% Run the test case.
		{H,ParH,Res} = 
		    run_parallel_commands(?MODULE,{Seq,Par}),
		?WHENFAIL(
		   dpretty:print(H,ParH,Res),
		   collect(length(Par),
			   aggregate([length(P) || P <- Par],
				     collect(length([ok || P <- Par,
							   {set,_,{call,_,open_file,_}} <- P]),
				     Res == ok %andalso not corrupted(dets_table,Type)
					    ))))
	    end))))
     ).

corrupted(T,Type) ->
    dets:open_file(T,[{type,Type}]),
    Ans = length(dets:match_object(T,'_')) =/= dets:info(T,no_objects),
    dets:close(T),
    Ans.


open_file(Name,Args) ->
    {ok,N} = dets:open_file(Name,Args),
    N.

%% Operations for inclusion in test cases

get_contents(Name) ->
    dets:traverse(Name,fun(X)->{continue,X}end).
			       
file_delete(Name) ->
    case file:delete(Name) of
	{error,enoent} ->
	    ok;
	_Bad ->
	    file_delete(Name)
    end.

task_3_1() ->
  [{set, {var, 1}, {call, dets_eqc, open_file,    [dets_table, [{type, bag}]]}},
   {set, {var, 2}, {call, dets, insert,           [dets_table, {2, 7}]}},
   {set, {var, 3}, {call, dets, lookup,           [dets_table, 2]}},
   {set, {var, 4}, {call, dets, close,            [dets_table]}},
   {set, {var, 5}, {call, dets_eqc, open_file,    [dets_table, [{type, bag}]]}},
   {set, {var, 6}, {call, dets, delete,           [dets_table, 4]}},
   {set, {var, 7}, {call, dets, lookup,           [dets_table, 2]}},
   {set, {var, 8}, {call, dets_eqc, get_contents, [dets_table]}},
   {set, {var, 9}, {call, dets, insert_new,       [dets_table, {2, 2}]}}].

task_3_2() ->
  [{set, {var, 1},  {call, dets_eqc, open_file,    [dets_table, [{type, bag}]]}},
   {set, {var, 2},  {call, dets,     delete,       [dets_table, 7]}},
   {set, {var, 3},  {call, dets,     insert,       [dets_table, {1, 4}]}},
   {set, {var, 4},  {call, dets,     lookup,       [dets_table, 2]}},
   {set, {var, 5},  {call, dets,     lookup,       [dets_table, 1]}},
   {set, {var, 6},  {call, dets,     insert_new,   [dets_table, {1, 0}]}},
   {set, {var, 7},  {call, dets,     close,        [dets_table]}},
   {set, {var, 8},  {call, dets_eqc, open_file,    [dets_table, [{type, bag}]]}},
   {set, {var, 9},  {call, dets,     insert_new,   [dets_table, {2, 6}]}},
   {set, {var, 10}, {call, dets,     insert,       [dets_table, {3, 2}]}},
   {set, {var, 11}, {call, dets_eqc, get_contents, [dets_table]}}].

task_3_3() ->
  [{set, {var, 1},  {call, dets_eqc, open_file,    [dets_table, [{type, bag}]]}},
   {set, {var, 2},  {call, dets,     delete,       [dets_table, 7]}},
   {set, {var, 3},  {call, dets,     insert_new,   [dets_table, {1, 4}]}},
   {set, {var, 4},  {call, dets,     lookup,       [dets_table, 1]}},
   {set, {var, 5},  {call, dets,     insert_new,   [dets_table, {1, 5}]}},
   {set, {var, 6},  {call, dets,     lookup,       [dets_table, 3]}},
   {set, {var, 7},  {call, dets_eqc, get_contents, [dets_table]}},
   {set, {var, 8},  {call, dets,     insert_new,   [dets_table, {2, 4}]}},
   {set, {var, 9},  {call, dets_eqc, get_contents, [dets_table]}},
   {set, {var, 10}, {call, dets,     delete,       [dets_table, 1]}},
   {set, {var, 11}, {call, dets_eqc, get_contents, [dets_table]}}].
