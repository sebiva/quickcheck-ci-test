-module(bank_eqc).

-compile(export_all).
-compile({parse_transform, eqc_cover}).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-record(state, {open = false :: boolean(),
                users = [] :: [{atom(), atom()}],
                accounts = [] :: [{atom(), atom(), integer()}],
                logged_in = [] :: [atom()] }).

-define(LOW, 1).
-define(HIGH, 5).

-define(NAMES, [{?LOW, arne}, {?LOW, bengt}]).
-define(PWDS, [{?LOW, abc123}, {?LOW, pwd}]).
-define(ACCOUNTS, [{?LOW, savings}, {?LOW, credit}]).

-generate_examples(prop_bank).

name() -> frequency(?NAMES).
name(S) ->
  InState = [{?HIGH, Name} || {Name, _} <- S#state.users],
  frequency(InState ++ ?NAMES).
pwd() -> frequency(?PWDS).
pwd(S) ->
  InState = [{?HIGH, Pwd} || {_, Pwd} <- S#state.users],
  frequency(InState ++ ?PWDS).
account() -> frequency(?ACCOUNTS).
account(S) ->
  InState = [{?HIGH, Account} || {Account, _, _} <- S#state.accounts],
  frequency(InState ++ ?ACCOUNTS).

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
  bank:create_user(Name, Pwd).

create_user_next(S, _R, [Name, Pwd]) ->
  case create_user_ok(S, Name) of
    false -> S;
    _     -> S#state{users = [{Name, Pwd} | S#state.users]}
  end.

create_user_pre(S) ->
  S#state.open.

create_user_post(S, [Name, Pwd], R) ->
  case create_user_ok(S, Name) of
    true -> R == {Name, Pwd};
    _ ->    R == false
  end.

create_user_ok(S, Name) ->
  case lists:keyfind(Name, 1, S#state.users) of
    false -> true;
    _     -> false
  end.


create_account_args(S) ->
  [account(S), name(S)].

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
  S#state.open.

create_account_post(S, [AName, UName], R) ->
  Account = {AName, UName},
  case create_account_ok(S, Account) of
    true -> R == Account;
    false -> R == false
  end.

create_account_ok(S, {AName, Name}) ->
  logged_in(Name, S) andalso
    lists:filter(fun({AN, UN, _B}) -> AN == AName andalso UN == Name end, S#state.accounts) == [].


login_args(S) ->
  [name(S), pwd(S)].

login(Name, Pwd) ->
  bank:login(Name, Pwd).

login_pre(S) ->
  S#state.open.

login_next(S, _R, [Name, Pwd]) ->
  case pwd_ok(Name, Pwd, S) of
    false -> S;
    true -> case logged_in(Name, S) of
              true -> S;
              false -> S#state{logged_in = [Name | S#state.logged_in] }
            end
  end.

login_post(S, [Name, Pwd], R) ->
  case R of
    false -> logged_in(Name, S) or not exists(Name, S) or not pwd_ok(Name, Pwd, S);
    ok    -> not logged_in(Name, S) andalso
               pwd_ok(Name, Pwd, S)
  end.


logout_args(S) ->
  [name(S)].

logout(Name) ->
  bank:logout(Name).

logout_pre(S) ->
  S#state.open.

logout_next(S, _R, [Name]) ->
  case logout_ok(S, Name) of
    true  -> S#state{logged_in = S#state.logged_in -- [Name]};
    false -> S
  end.

logout_post(S, [Name], R) ->
  case logout_ok(S, Name) of
    true  -> R == ok;
    false -> R == false
  end.

logout_ok(S, Name) ->
  lists:member(Name, S#state.logged_in).


deposit_args(S) ->
  [name(S), pwd(S), account(S), choose(10, 1000)].

deposit(Name, Pwd, Account, Amount) ->
  bank:deposit(Name, Pwd, Account, Amount).

deposit_pre(S) ->
  S#state.open.

deposit_next(S, _R, [Name, Pwd, Account, Amount]) ->
  case deposit_ok(S, Name, Pwd, Account) of
    OldAcc = {AN, UN, Bal} -> NewBal = Bal + Amount,
                              S#state{accounts = (S#state.accounts -- [OldAcc]) ++ [{AN, UN, NewBal}]};
    false -> S
  end.

deposit_post(S, [Name, Pwd, Account, _Amount], R) ->
  case deposit_ok(S, Name, Pwd, Account) of
    false -> R == false;
    _     -> R == ok
  end.

deposit_ok(S, Name, Pwd, Account) ->
  case logged_in(Name, S) andalso pwd_ok(Name, Pwd, S) of
    true -> case lists:filter(fun({AN, UN, _B}) -> AN == Account andalso UN == Name end, S#state.accounts) of
              [OldAcc] -> OldAcc;
              _        -> false
            end;
    false -> false
  end.


withdraw_args(S) ->
  [name(S), pwd(S), account(S), choose(10, 1000)].

withdraw(Name, Pwd, Account, Amount) ->
  bank:withdraw(Name, Pwd, Account, Amount).

withdraw_pre(S) ->
  S#state.open.

withdraw_next(S, _R, [Name, Pwd, Account, Amount]) ->
  case withdraw_ok(S, Name, Pwd, Account, Amount) of
    OldAcc = {AN, UN, Bal} -> NewBal = Bal - Amount,
                              S#state{accounts = (S#state.accounts -- [OldAcc]) ++ [{AN, UN, NewBal}]};
    false -> S
  end.

withdraw_post(S, [Name, Pwd, Account, Amount], R) ->
  case withdraw_ok(S, Name, Pwd, Account, Amount) of
    false -> R == false;
    _     -> R == ok
  end.

withdraw_ok(S, Name, Pwd, Account, Amount) ->
  case logged_in(Name, S) andalso pwd_ok(Name, Pwd, S) of
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

pwd_ok(Name, Pwd, S) ->
  lists:member({Name, Pwd}, S#state.users).

prop_bank() ->
  ?FORALL(Commands, commands(?MODULE),
          begin
            gen_server:start({global, bank}, bank, [], []),
            {H, S, Res} = run_commands(?MODULE, Commands),  % {History, Final model State, Result}
            catch gen_server:stop({global, bank}),
            Negative = [false],
            find_examples:generate_examples(?MODULE, Commands, H, Res, Negative,
                pretty_commands(?MODULE, Commands, {H, S, Res},
                                aggregate(command_names(Commands),
                                      Res == ok)))
          end).

prop_cover() ->
  ?FORALL(Commands, ex_cover:gen_commands(?MODULE),
          begin
            gen_server:start({global, bank}, bank, [], []),
            Prop = fun(_H, _S, Res) -> catch gen_server:stop({global, bank}),
                                     Res == ok
                   end,
            ex_cover:ex_coverage(?MODULE, Commands, Prop)
          end).

prop_swap() ->
  ?FORALL(SwapCmds, ex_swap:gen_swapcommands(?MODULE),
          begin
            Cmds = ex_swap:get_commands(SwapCmds),
            gen_server:start({global, bank}, bank, [], []),
            {H, _S, Res} = run_commands(?MODULE, Cmds),
            catch gen_server:stop({global, bank}),
            ex_swap:interesting(?MODULE, SwapCmds, H, Res)
          end).

prop_param() ->
  ?FORALL(Cmds, find_examples:gen_commands_statem(?MODULE),
          begin
            gen_server:start({global, bank}, bank, [], []),
            {H, S, Res} = run_commands(?MODULE, Cmds),
            catch gen_server:stop({global, bank}),
            Negative = [false],
            find_examples:generate_examples(?MODULE, Cmds, H, Res, Negative,
                pretty_commands(?MODULE, Cmds, {H, S, Res},
                                aggregate(command_names(Cmds),
                                      Res == ok)))
          end).

prop_a() ->
  ?TIMEOUT(2,
  ?FORALL(Cmds, commands(?MODULE),
          begin
            gen_server:start({global, bank}, bank, [], []),
            {H, _, Res} = run_commands(?MODULE, Cmds),
            catch gen_server:stop({global, bank}),
            %eqc:features([length(H)], eqc:numtests(1, (Res == ok or (length(H) < 3))))
            eqc:features([length(H)], eqc:numtests(1, (Res == ok) or (length(H) < 4)))
          end)).


