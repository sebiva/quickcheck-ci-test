
%%% File    : q_eqc.erl
%%% Author  :  <>
%%% Description : 
%%% Created :  7 Oct 2009 by  <>

-module(q_eqc).


-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").

-compile(export_all).

-record(state,{ptr,size,elements}).

-generate_examples(prop_q).

%% Definition of the states. Each state is represented by a function,
%% listing the transitions from that state, together with generators
%% for the calls to make each transition.
init_state(_S) ->
    [ {created,{call,q,new,[nat()]}}
     ].

created(S) ->
    [ {created,{call,q,put,[S#state.ptr,int()]}}
    , {created,{call,q,get,[S#state.ptr]}}
    , {created, {call,q,size,[S#state.ptr]}}
    ].

%% Identify the initial state
initial_state() ->
    init_state.

%% Initialize the state data
initial_state_data() ->
    #state{}.

%% Next state transformation for state data.
%% S is the current state, From and To are state names
next_state_data(_From,_To,S,Ptr,{call,_,new,[Size]}) ->
    S#state{ptr=Ptr,size=Size,elements=[]};
next_state_data(_From,_To,S,_V,{call,_,put,[_,X]}) ->
    S#state{elements=S#state.elements++[X]};
next_state_data(_From,_To,S,_V,{call,_,get,_}) ->
    S#state{elements=tl(S#state.elements)};
next_state_data(_From,_To,S,_V,{call,_,_,_}) ->
    S.

%% Precondition (for state data).
%% Precondition is checked before command is added to the command sequence
precondition(_From,_To,S,{call,_,get,_}) ->
    S#state.elements /= [];
precondition(_From,_To,S,{call,_,put,_}) ->
    length(S#state.elements) < S#state.size;
precondition(_From,_To,_S,{call,_,new,[Size]}) ->
    Size > 0;
precondition(_From,_To,_S,{call,_,_,_}) ->
    true.

%% Postcondition, checked after command has been evaluated
%% OBS: S is the state before next_state_data(From,To,S,_,<command>)
postcondition(_From,_To,S,{call,_,get,_},Res) ->
    Res == hd(S#state.elements);
postcondition(_From,_To,S,{call,_,size,_},Res) ->
    Res == length(S#state.elements);
postcondition(_From,_To,_S,_Call,_Res) ->
    true.

prop_q() ->
    eqc_c:start(q, [definitions_only, {c_src, "../src/q.c"}]),
    ?FORALL(Cmds,commands(?MODULE),
      prop_q(Cmds)).

prop_q(Cmds) ->
    {H,S,Res} = run_commands(?MODULE,Cmds),
    find_examples:generate_examples(?MODULE, Cmds, H, Res,
                                    pretty_commands(?MODULE, Cmds, {H, S, Res},
                                                    aggregate(command_names(Cmds),
                                                              Res == ok))).

%% Weight for transition (this callback is optional).
%% Specify how often each transition should be chosen
weight(_From,_To,_Call) ->
    1.

bugs() ->
    bugs:print_bugs(bugs:bugs_fsm(?MODULE, fun prop_q/1)).

refine() ->
    bugs:refine_fsm(?MODULE, fun prop_q/1).

print_stuff() ->
    Examples = lists:concat(examples:find_examples(prop_q())),
    [ begin print_examples:print(eqc_fsm_callbacks:new(?MODULE), Cmds), io:format("~n") end || Cmds <- Examples ].

features() ->
  [{[{feature,postcondition_failed,
      {set,'%_',{'$call',q,get,[{'%var',1}]}},
      {set,'%_',{'$call',q,get,[{'%var',1}]}},
      {[0],[0]}}],
    [[{set,{var,1},{call,q,new,[2]}},
      {set,{var,15},{call,q,put,[{var,1},1]}},
      {set,{var,17},{call,q,put,[{var,1},0]}},
      {set,{var,18},{call,q,get,[{var,1}]}},
      {set,{var,19},{call,q,get,[{var,1}]}}],
     [{command,{set,{var,1},{call,q,new,[2]}},
       {ptr,"Queue",36111856}},
      {command,{set,{var,15},{call,q,put,[{var,1},1]}},ok},
      {command,{set,{var,17},{call,q,put,[{var,1},0]}},ok},
      {command,{set,{var,18},{call,q,get,[{var,1}]}},1},
      {command,{set,{var,19},{call,q,get,[{var,1}]}},0}],
     {example,4,5}]},
   {[{feature,postcondition_failed,
      {set,'%_',{'$call',q,put,[{'%var',1},'%_']}},
      {set,'%_',{'$call',q,get,[{'%var',1}]}},
      {[0],[0]}}],
    [[{set,{var,1},{call,q,new,[2]}},
      {set,{var,2},{call,q,put,[{var,1},0]}},
      {set,{var,3},{call,q,put,[{var,1},1]}},
      {set,{var,4},{call,q,get,[{var,1}]}}],
     [{command,{set,{var,1},{call,q,new,[2]}},
       {ptr,"Queue",36109936}},
      {command,{set,{var,2},{call,q,put,[{var,1},0]}},ok},
      {command,{set,{var,3},{call,q,put,[{var,1},1]}},ok},
      {command,{set,{var,4},{call,q,get,[{var,1}]}},0}],
     {example,2,4}]},
   {[{feature,precondition_failed,
      {set,'%_',{'$call',q,get,[{'%var',1}]}},
      {set,'%_',{'$call',q,put,[{'%var',1},'%_']}},
      {[0],[0]}}],
    [[{set,{var,1},{call,q,new,[1]}},
      {set,{var,7},{call,q,put,[{var,1},0]}},
      {set,{var,8},{call,q,get,[{var,1}]}},
      {set,{var,9},{call,q,put,[{var,1},0]}}],
     [{command,{set,{var,1},{call,q,new,[1]}},
       {ptr,"Queue",36108400}},
      {command,{set,{var,7},{call,q,put,[{var,1},0]}},ok},
      {command,{set,{var,8},{call,q,get,[{var,1}]}},0},
      {command,{set,{var,9},{call,q,put,[{var,1},0]}},ok}],
     {example,3,4}]},
   {[{feature,postcondition_failed,
      {set,'%_',{'$call',q,get,[{'%var',1}]}},
      {set,'%_',{'$call',q,size,[{'%var',1}]}},
      {[0],[0]}}],
    [[{set,{var,1},{call,q,new,[1]}},
      {set,{var,4},{call,q,put,[{var,1},0]}},
      {set,{var,5},{call,q,get,[{var,1}]}},
      {set,{var,6},{call,q,size,[{var,1}]}}],
     [{command,{set,{var,1},{call,q,new,[1]}},
       {ptr,"Queue",36106928}},
      {command,{set,{var,4},{call,q,put,[{var,1},0]}},ok},
      {command,{set,{var,5},{call,q,get,[{var,1}]}},0},
      {command,{set,{var,6},{call,q,size,[{var,1}]}},0}],
     {example,3,4}]},
   {[{feature,postcondition_failed,
      {set,'%_',{'$call',q,put,[{'%var',1},'%_']}},
      {set,'%_',{'$call',q,size,[{'%var',1}]}},
      {[0],[0]}}],
    [[{set,{var,1},{call,q,new,[1]}},
      {set,{var,4},{call,q,put,[{var,1},0]}},
      {set,{var,6},{call,q,size,[{var,1}]}}],
     [{command,{set,{var,1},{call,q,new,[1]}},
       {ptr,"Queue",36106032}},
      {command,{set,{var,4},{call,q,put,[{var,1},0]}},ok},
      {command,{set,{var,6},{call,q,size,[{var,1}]}},1}],
     {example,2,3}]},
   {[{feature,precondition_failed,
      {set,'%_',{'$call',q,put,[{'%var',1},'%_']}},
      {set,'%_',{'$call',q,get,[{'%var',1}]}},
      {[0],[0]}}],
    [[{set,{var,1},{call,q,new,[1]}},
      {set,{var,7},{call,q,put,[{var,1},0]}},
      {set,{var,8},{call,q,get,[{var,1}]}}],
     [{command,{set,{var,1},{call,q,new,[1]}},
       {ptr,"Queue",36105200}},
      {command,{set,{var,7},{call,q,put,[{var,1},0]}},ok},
      {command,{set,{var,8},{call,q,get,[{var,1}]}},0}],
     {example,2,3}]}].

task_1_1() ->
  [{set, {var, 1}, {call, q, new,  [9]}},
   {set, {var, 2}, {call, q, put,  [{var, 1}, 4]}},
   {set, {var, 3}, {call, q, size, [{var, 1}]}},
   {set, {var, 4}, {call, q, put,  [{var, 1}, 3]}}].

task_1_2() ->
  [{set, {var, 1}, {call, q, new,  [3]}},
   {set, {var, 2}, {call, q, size, [{var, 1}]}},
   {set, {var, 4}, {call, q, put,  [{var, 1}, 6]}},
   {set, {var, 4}, {call, q, get,  [{var, 1}]}},
   {set, {var, 4}, {call, q, put,  [{var, 1}, -10]}},
   {set, {var, 5}, {call, q, size, [{var, 1}]}},
   {set, {var, 6}, {call, q, size, [{var, 1}]}},
   {set, {var, 7}, {call, q, get,  [{var, 1}]}},
   {set, {var, 8}, {call, q, size, [{var, 1}]}},
   {set, {var, 9}, {call, q, put,  [{var, 1}, -8]}},
   {set, {var, 2}, {call, q, size, [{var, 1}]}},
   {set, {var, 2}, {call, q, put,  [{var, 1}, -10]}},
   {set, {var, 2}, {call, q, size, [{var, 1}]}},
   {set, {var, 2}, {call, q, get,  [{var, 1}]}},
   {set, {var, 2}, {call, q, get,  [{var, 1}]}}].

task_1_3() ->
  [{set, {var, 1}, {call, q, new,  [2]}},
   {set, {var, 2}, {call, q, size, [{var, 1}]}},
   {set, {var, 4}, {call, q, put,  [{var, 1}, 6]}},
   {set, {var, 4}, {call, q, put,  [{var, 1}, -10]}},
   {set, {var, 5}, {call, q, size, [{var, 1}]}},
   {set, {var, 7}, {call, q, get,  [{var, 1}]}},
   {set, {var, 8}, {call, q, size, [{var, 1}]}},
   {set, {var, 9}, {call, q, put,  [{var, 1}, -8]}},
   {set, {var, 2}, {call, q, put,  [{var, 1}, -10]}}].
