-module(bank).
-compile({parse_transform, eqc_cover}).

%              {bool(), [{{name(), acc_name()}, int()}], [{name(), pwd()}], [name()]}
-record(state, {open = false, accounts = [], customers = [], logged_in = []}).

-export([launch/0,stop/0,
         init/1,terminate/2,handle_call/3, handle_cast/2, handle_info/2,code_change/3,
         open/0, close/0, create_user/2, create_account/2, login/2, logout/1, deposit/4, withdraw/4]).

%%%% Gen server code
-behaviour(gen_server).

init(_Args) ->
  {ok, #state{}}.

launch() ->
  gen_server:start({global, bank}, bank, [], []).

stop() ->
  gen_server:stop({global, bank}).

terminate(_Reason, _State) ->
  ok.

code_change(_,S,_) -> {ok, S}.

handle_call(Req, _From, State) when not State#state.open ->
  {NewState, Res} = case Req of
    open -> open(State);
    _    -> {State, false}
  end,
  {reply, Res, NewState};
handle_call(Req, _From, State) ->
  {NewState, Res} = case Req of
    open -> open(State);
    close -> close(State);
    {login, {Name, Pwd}} -> login(Name, Pwd, State);
    {logout, Name} -> logout(Name, State);
    {create_user, {Name, Pwd}} ->
      create_user(Name, Pwd, State);
    {create_account, AccountName, UserName} ->
      create_account(AccountName, UserName, State);
    {deposit, {Name, Pwd}, Account, Amount} ->
      deposit(Name, Pwd, Account, Amount, State);
    {withdraw, {Name, Pwd}, Account, Amount} ->
      withdraw(Name, Pwd, Account, Amount, State)
  end,
  {reply, Res, NewState}.

handle_cast(Req, State) ->
  {reply, Res, NewState} = handle_call(Req, self(), State),
  {noreply, Res, NewState}.

handle_info(_Info, State) ->
  {noreply, State}.

open() -> gen_server:call({global, bank}, open).
close() -> gen_server:call({global, bank}, close).
create_user(Name, Pwd) ->
  gen_server:call({global, bank}, {create_user, {Name, Pwd}}).
create_account(AccountName, UserName) ->
  gen_server:call({global, bank}, {create_account, AccountName, UserName}).
login(Name, Pwd) ->
  gen_server:call({global, bank}, {login, {Name, Pwd}}).
logout(Name) ->
  gen_server:call({global, bank}, {logout, Name}).
deposit(Name, Pwd, Account, Amount) ->
  gen_server:call({global, bank}, {deposit, {Name, Pwd}, Account, Amount}).
withdraw(Name, Pwd, Account, Amount) ->
  gen_server:call({global, bank}, {withdraw, {Name, Pwd}, Account, Amount}).

%%%%% Program code

open(State) when not State#state.open ->
  {State#state{open=true}, ok};
open(State) ->
  {State, false}.

close(State) when State#state.open ->
  {State#state{open=false, logged_in=[]}, ok};
close(State) ->
  {State, false}.

create_user(Name, Pwd, State) ->
  User = {Name, Pwd},
  case user_exists(Name, State) of
    false ->
      {State#state{customers = [User | State#state.customers]}, User};
    _     ->
      {State, false}
  end.

user_exists(Name, State) ->
  case lists:keyfind(Name, 1, State#state.customers) of
    false -> false;
    _     -> true
  end.

create_account(AccountName, UserName, State) ->
  Account = {AccountName, UserName},
  case logged_in(UserName, State) of
    true ->
      Accounts = State#state.accounts,
      case lists:filter(fun({{A, U}, _B})
                          -> {A, U} == Account end, Accounts) of
        [] -> {State#state{accounts = [{Account, 0} | Accounts]}, Account};
        _  -> {State, false}
      end;
    false -> {State, false}
  end.

logged_in(Name, State) ->
  lists:member(Name, State#state.logged_in).

login(Name, Pwd, State) ->
  case lists:member({Name, Pwd}, State#state.customers) of
    false -> {State, false};
    true  ->
      case logged_in(Name, State) of
        true -> {State, false};
        false -> {State#state{logged_in=[Name|State#state.logged_in]}, ok}
      end
  end.

logout(Name, State) ->
  case lists:member(Name, State#state.logged_in) of
    false -> {State, false};
    _     -> {State#state{logged_in=State#state.logged_in -- [Name]}, ok}
  end.

pwd_ok(User = {Name, _Pwd}, State) ->
  lists:member(User, State#state.customers) andalso logged_in(Name, State).

deposit(Name, Pwd, Account, Amount, State) ->
  case pwd_ok({Name, Pwd}, State) of
    true ->
      case lists:keyfind({Account, Name}, 1, State#state.accounts) of
        OldAccount = {N, Balance} ->
          NewBalance = Balance + Amount,
          {State#state{accounts=(State#state.accounts -- [OldAccount]) ++
                       [{N, NewBalance}]}, NewBalance};
        _                   -> {State, false}
      end;
    false -> {State, false}
  end.

withdraw(Name, Pwd, Account, Amount, State) ->
  case pwd_ok({Name, Pwd}, State) of
    true ->
      case lists:keyfind({Account, Name}, 1, State#state.accounts) of
        OldAccount = {N, Balance} when Balance >= Amount ->
          NewBalance = Balance - Amount,
          {State#state{accounts=(State#state.accounts -- [OldAccount]) ++
                       [{N, NewBalance}]}, NewBalance};
        _ -> {State, false}
      end;
    false -> {State, false}
  end.


