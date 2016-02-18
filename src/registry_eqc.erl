-module(registry_eqc).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-record(state, {registered = [], pids = [], dead = []}).

%-behaviour(eqc_statem).

-define(NAMES, [a,b,c,d]).

initial_state() ->
  #state{registered = []}.

name() ->
  elements(?NAMES).

pid(S) ->
  elements(S#state.pids).

% register
register_args(S) ->
  [name(), pid(S)].

register(Name, Pid) ->
  % Clean up the exception that is caught to make the printed output nicer
  case catch erlang:register(Name, Pid) of
    {'EXIT', {badarg, [{erlang, register, Args, _} | _]}} -> {'EXIT', badarg, {erlang, register, Args}};
    Res                                                    -> Res
  end.

register_next(S, _R, [Name, Pid]) ->
  case register_ok(S, [Name, Pid]) of
    false ->
      S;
    true ->
      S#state{registered = [{Name, Pid} | S#state.registered]}
  end.

register_pre(S) ->
  length(S#state.pids) > 0.

register_post(S, [Name, Pid], R) ->
  case register_ok(S, [Name, Pid]) of
    true ->
      eq(R, true);
    false ->
      is_exit(R)
  end.

register_ok(S, [Name, Pid]) ->
  not lists:keymember(Name, 1,  S#state.registered) andalso
    not lists:keymember(Pid, 2,  S#state.registered) andalso
      not lists:member(Pid, S#state.dead).

% unregister
% TODO: Negative testing
unregister_args(_S) ->
  [name()].

unregister(Name) ->
  erlang:unregister(Name).

unregister_next(S, _R, [Name]) ->
  S.
  %S#state{registered = lists:keydelete(Name, 1, S#state.registered)}.

unregister_pre(S, [Name]) ->
  lists:keymember(Name, 1, S#state.registered).

% whereis
whereis_args(_S) ->
  [name()].

whereis(Name) ->
  erlang:whereis(Name).

whereis_post(S, [Name], R) ->
  case lists:keyfind(Name, 1, S#state.registered) of
    {Name, Pid} ->
      R == Pid;
    false ->
      R == undefined
  end.

spawn_process_args(_S) ->
  [].

spawn_process_next(S, R, []) ->
  S#state{pids = [R | S#state.pids]}.

spawn_process() ->
  spawn(fun() ->
            receive
              _ -> ok
            end
        end).

kill_process_args(S) ->
  [pid(S)].

kill_process(Pid) ->
  exit(Pid, kill),
  timer:sleep(1).

kill_process_next(S, _R, [Pid]) ->
  S#state{dead = [Pid | S#state.dead], registered = lists:keydelete(Pid, 2, S#state.registered)}.

kill_process_pre(S) ->
  S#state.pids /= [].

kill_process_pre(S, [Pid]) ->
  lists:member(Pid, S#state.pids).

is_exit({'EXIT', _, _}) ->
  true;
is_exit(_) ->
  false.


prop_reg() ->
  ?FORALL(Commands, commands(?MODULE),
          begin
            [catch erlang:unregister(Name) || Name <- ?NAMES],
            {H, S, Res} = run_commands(?MODULE, Commands),  % {History, Final model State, Result}
            pretty_commands(?MODULE, Commands, {H, S, Res},
                            aggregate(command_names(Commands),
                                      Res == ok))
          end).


prop_reg_ex() ->
  ?FORALL(Cmds,commands(?MODULE),
          begin
            [catch erlang:unregister(N) || N <- ?NAMES],
            {H, S, Res} = run_commands(?MODULE, Cmds),
            find_examples:generate_examples(?MODULE, Cmds, H,
                                            pretty_commands(?MODULE, Cmds, {H, S, Res},
                                                            aggregate(command_names(Cmds),
                                                                      Res == ok)))
          end).