-module(bank_eqc).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

%              {bool(), [{name(), acc_name(), int()}], [{name(), pwd()}]}
-record(state, {open = false, accounts = [], customers = []}).

-define(NAMES, [arne, bengt, carl, david]).
%-define(NAMES, [arne]).
-define(PWDS, [abc123, pwd, '12345', '42']).
-define(ACCOUNTS, [savings, credit, secret]).

name() -> elements(?NAMES).
pwd() -> elements(?PWDS).
account() -> elements(?ACCOUNTS).

is_exit({'EXIT', _}) ->
  true;
is_exit(_) ->
  false.

initial_state() ->
  #state{}.

open_args(_S) -> [].
open() -> bank:open().
open_pre(S) -> not S#state.open.
open_next(S, _R, []) -> S#state{open = true}.
open_post(_S, [], R) -> R == ok.

close_args(_S) -> [].
close() -> bank:close().
close_pre(S) -> S#state.open.
close_next(S, _R, []) -> S#state{open = false}.
close_post(_S, [], R) -> R == ok.


create_user_args(_S) ->
  [name(), pwd()].

create_user(Name, Pwd) ->
  bank:create_user({Name, Pwd}).

create_user_next(S, _R, [Name, Pwd]) ->
  case create_user_ok(S, Name) of
    false -> S;
    _     -> S#state{customers = [{Name, Pwd} | S#state.customers]}
  end.

create_user_pre(S) ->
  S#state.open.

create_user_post(S, [Name, _Pwd], R) ->
  %io:format("Result: ~p~nState: ~p~n", [R, S]),
  case R of
    false -> create_user_ok(S, Name) == false;
    _ -> true
  end.

create_user_ok(S, Name) ->
  case lists:keyfind(Name, 1, S#state.customers) of
    false -> true;
    _     -> false
  end.

create_account_args(S) ->
  %Names = [N || {N, _P} <- S#state.customers],
  %io:format("Picking Name: ~p~n", [Names]),
  [account(), elements(?NAMES)].

create_account(AccountName, Name) ->
  bank:create_account(AccountName, Name).

create_account_next(S, _R, [AName, UName]) ->
  case create_account_ok(S, {AName, UName}) of
    false -> S;
    _     -> S#state{accounts = [{UName, AName, 0} | S#state.accounts]}
  end.

create_account_pre(S) ->
  %io:format("Account pre: ~p~n", [S]),
  S#state.customers /= [] andalso
    S#state.open.

create_account_post(S, [AName, UName], R) ->
  case R of
    false -> create_account_ok(S, {AName, UName}) == false;
    _     -> true
  end.

create_account_ok(S, {AName, UName}) ->
  UserExists = case lists:filter(fun({Name, _Pwd}) -> Name == UName end, S#state.customers) of
    [] -> false;
    _  -> true
  end,
  LoggedIn = true, % TODO
  AccountExists = case lists:filter(fun({U, A, _B}) -> A == AName andalso U == UName end, S#state.accounts) of
    [] -> false;
    _  -> true
  end,
  UserExists andalso LoggedIn andalso AccountExists.


prop_bank() ->
  ?FORALL(Commands, commands(?MODULE),
          begin
            gen_server:start({global, bank}, bank, [], []),
            {H, S, Res} = run_commands(?MODULE, Commands),  % {History, Final model State, Result}
            catch gen_server:stop({global, bank}),
            pretty_commands(?MODULE, Commands, {H, S, Res},
                            aggregate(command_names(Commands),
                                      Res == ok))
          end).


