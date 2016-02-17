-module(bank_eqc).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-record(state, {open = false :: boolean(),
                users = [] :: [{atom(), atom()}],
                accounts = [] :: [{atom(), atom(), integer()}],
                logged_in = [] :: [atom()] }).

-define(NAMES, [arne, bengt]).
-define(PWDS, [abc123, pwd]).
-define(ACCOUNTS, [savings, credit]).

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
close_next(S, _R, []) -> S#state{open = false, logged_in = []}.
close_post(_S, [], R) -> R == ok.


create_user_args(_S) ->
  [name(), pwd()].

create_user(Name, Pwd) ->
  bank:create_user({Name, Pwd}).

create_user_next(S, _R, [Name, Pwd]) ->
  case create_user_ok(S, Name) of
    false -> S;
    _     -> S#state{users = [{Name, Pwd} | S#state.users]}
  end.

create_user_pre(S) ->
  S#state.open.

create_user_post(S, [Name, _Pwd], R) ->
  case R of
    false -> create_user_ok(S, Name) == false;
    _ -> true
  end.

create_user_ok(S, Name) ->
  case lists:keyfind(Name, 1, S#state.users) of
    false -> true;
    _     -> false
  end.


create_account_args(_S) ->
  %Names = [N || {N, _P} <- S#state.customers],
  % TODO: Should the names be picked from customers only?
  [account(), name()].

create_account(AccountName, Name) ->
  bank:create_account(AccountName, Name).

create_account_next(S, _R, [AName, UName]) ->
  SS = case create_account_ok(S, {AName, UName}) of
    true ->
      S#state{accounts = [{AName, UName, 0} | S#state.accounts]};
    false ->
      S
  end,
  SS.

create_account_pre(S) ->
  %S#state.logged_in /= [] andalso
    S#state.open.

create_account_post(S, [AName, UName], R) ->
  case R of
    false -> create_account_ok(S, {AName, UName}) == false;
    _     -> true
  end.

create_account_ok(S, {AName, UName}) ->
  logged_in(UName, S) andalso
    lists:filter(fun({AN, UN, _B}) -> AN == AName andalso UN == UName end, S#state.accounts) == [].


login_args(_S) ->
  %Names = [Name || {Name, _} <- S#state.users],
  %[{elements(Names), pwd()}].
  [{name(), pwd()}].

login(User) ->
  bank:login(User).

login_pre(S) ->
  S#state.open.
  %S#state.open andalso
    %S#state.users /= [].

login_next(S, _R, [User = {Name, _Pwd}]) ->
  case pwd_ok(User, S) of
    false -> S;
    true -> case logged_in(Name, S) of
              true -> S;
              false -> S#state{logged_in = [Name | S#state.logged_in] }
            end
  end.

login_post(S, [User = {Name, Pwd}], R) ->
  case R of
    false -> logged_in(Name, S) or not exists(Name, S) or not pwd_ok({Name, Pwd}, S);
    ok    -> not logged_in(Name, S) andalso
               pwd_ok(User, S)
  end.


logout_args(_S) ->
  [name()].
  %[elements(S#state.logged_in)].

logout(Name) ->
  bank:logout(Name).

logout_pre(S) ->
  S#state.open. % andalso
    %S#state.logged_in /= [].

logout_next(S, _R, [Name]) ->
  case logout_ok(S, Name) of
    true  -> S#state{logged_in = S#state.logged_in -- [Name]};
    false -> S
  end.

logout_post(S, [Name], R) ->
  case logout_ok(S, Name) of
    true  -> R == ok;
    false -> true
  end.

logout_ok(S, Name) ->
  lists:member(Name, S#state.logged_in).


deposit_args(_S) ->
  [{name(), pwd()}, account(), choose(10, 1000)].

deposit(User, Account, Amount) ->
  bank:deposit(User, Account, Amount).

deposit_pre(S) ->
  S#state.open.

deposit_next(S, _R, [User, Account, Amount]) ->
  case deposit_ok(S, User, Account) of
    OldAcc = {AN, UN, Bal} -> NewBal = Bal + Amount,
                              S#state{accounts = (S#state.accounts -- [OldAcc]) ++ [{AN, UN, NewBal}]};
    false -> S
  end.

deposit_post(S, [User, Account, _Amount], R) ->
  case deposit_ok(S, User, Account) of
    false -> R == false;
    _     -> R == ok
  end.

deposit_ok(S, User = {Name, _Pwd}, Account) ->
  case logged_in(Name, S) andalso pwd_ok(User, S) of
    true -> case lists:filter(fun({AN, UN, _B}) -> AN == Account andalso UN == Name end, S#state.accounts) of
              [OldAcc] -> OldAcc;
              _        -> false
            end;
    false -> false
  end.


withdraw_args(_S) ->
  [{name(), pwd()}, account(), choose(10, 1000)].

withdraw(User, Account, Amount) ->
  bank:withdraw(User, Account, Amount).

withdraw_pre(S) ->
  S#state.open.

withdraw_next(S, _R, [User, Account, Amount]) ->
  case withdraw_ok(S, User, Account, Amount) of
    OldAcc = {AN, UN, Bal} -> NewBal = Bal - Amount,
                              S#state{accounts = (S#state.accounts -- [OldAcc]) ++ [{AN, UN, NewBal}]};
    false -> S
  end.

withdraw_post(S, [User, Account, Amount], R) ->
  false.
  %case withdraw_ok(S, User, Account, Amount) of
    %false -> R == false;
    %_     -> R == ok
  %end.

withdraw_ok(S, User = {Name, _Pwd}, Account, Amount) ->
  case logged_in(Name, S) andalso pwd_ok(User, S) of
    true -> case lists:filter(fun({AN, UN, _B}) -> AN == Account andalso UN == Name end, S#state.accounts) of
              [OldAcc = {_AN, _UN, Bal}] when Bal >= Amount -> OldAcc;
              _        -> false
            end;
    false -> false
  end.

logged_in(Name, S) ->
  lists:member(Name, S#state.logged_in).

exists(Name, S) ->
  lists:member(Name, lists:map(fun({N,_P}) -> N end, S#state.users)).

pwd_ok(User, S) ->
  lists:member(User, S#state.users).

prop_bank() ->
  ?FORALL(Commands, commands(?MODULE),
          begin
            gen_server:start({global, bank}, bank, [], []),
            {H, S, Res} = run_commands(?MODULE, Commands),  % {History, Final model State, Result}
            catch gen_server:stop({global, bank}),
            find_examples:generate_examples(?MODULE, Commands, H,
                pretty_commands(?MODULE, Commands, {H, S, Res},
                                aggregate(command_names(Commands),
                                      Res == ok)))
          end).

