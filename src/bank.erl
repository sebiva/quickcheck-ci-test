-module(bank).
-compile({parse_transform, eqc_cover}).

%              {bool(), [{{name(), acc_name()}, int()}], [{name(), pwd()}], [name()]}
-record(state, {open = false, accounts = [], customers = [], logged_in = []}).

-export([launch/0,stop/0,
         state/0, exists/2, logged_in/2, % For debugging only. TODO
         init/1,terminate/2,handle_call/3, handle_cast/2, handle_info/2,code_change/3,
         open/0, close/0, create_user/2, create_account/2, login/2, logout/1, deposit/4, withdraw/4]).

-behaviour(gen_server).

launch() ->
  gen_server:start({global, bank}, bank, [], []).

stop() ->
  gen_server:stop({global, bank}).

init(_Args) ->
  {ok, #state{}}.

terminate(_Reason, _State) ->
  ok.

handle_call(Req, _From, State) when not State#state.open ->
  {NewState, Res} = case Req of
    open -> {State#state{open = true}, ok};
    _    -> {State, false}
  end,
  {reply, Res, NewState};
handle_call(Req, _From, State) ->
  {NewState, Res} = case Req of
    close -> {State#state{open = false, logged_in = []}, ok};
    open -> {State, false};
    {create_user, {Name, Pwd}} -> create_user(Name, Pwd, State);
    {create_account, AccountName, UserName} -> create_account(AccountName, UserName, State);
    {login, {Name, Pwd}} -> login(Name, Pwd, State);
    {logout, Name} -> logout(Name, State);
    {deposit, {Name, Pwd}, Account, Amount} -> deposit(Name, Pwd, Account, Amount, State);
    {withdraw, {Name, Pwd}, Account, Amount} -> withdraw(Name, Pwd, Account, Amount, State);
    state -> {State, State} % For debugging
  end,
  {reply, Res, NewState}.

state() -> gen_server:call({global, bank}, state).

% TODO: Implement
code_change(_,S,_) -> {ok, S}.

handle_cast(Req, State) ->
  {reply, Res, NewState} = handle_call(Req, self(), State),
  {noreply, Res, NewState}.

handle_info(_Info, State) ->
  {noreply, State}.

open() -> gen_server:call({global, bank}, open).
close() -> gen_server:call({global, bank}, close).

create_user(Name, Pwd) -> gen_server:call({global, bank}, {create_user, {Name, Pwd}}).
create_user(Name, Pwd, State) ->
  User = {Name, Pwd},
  case exists(Name, State) of
    false -> {State#state{customers = [User | State#state.customers]}, User};
    _     -> {State, false}
  end.

exists(Name, State) ->
  case lists:keyfind(Name, 1, State#state.customers) of
    false -> false;
    _     -> true
  end.


create_account(AccountName, UserName) ->
  gen_server:call({global, bank}, {create_account, AccountName, UserName}).
create_account(AccountName, UserName, State) ->
  Account = {AccountName, UserName},
  case logged_in(UserName, State) of
    true -> Accounts = State#state.accounts,
            case lists:filter(fun({{A, U}, _B}) -> {A, U} == Account end,
                              Accounts) of
              [] -> {State#state{accounts = [{Account, 0} | Accounts]}, Account};
              _  -> {State, false}
            end;
    false -> {State, false}
  end.

logged_in(Name, State) ->
  lists:member(Name, State#state.logged_in).

login(Name, Pwd) ->
  gen_server:call({global, bank}, {login, {Name, Pwd}}).
login(Name, Pwd, State) ->
  case lists:member({Name, Pwd}, State#state.customers) of
    false -> {State, false};
    true  -> case logged_in(Name, State) of
               true -> {State, false};
               false -> {State#state{logged_in = [Name | State#state.logged_in]}, ok}
             end
  end.

logout(Name) ->
  gen_server:call({global, bank}, {logout, Name}).
logout(Name, State) ->
  case lists:member(Name, State#state.logged_in) of
    false -> {State, false};
    _     -> {State#state{logged_in = State#state.logged_in -- [Name]}, ok}
  end.

pwd_ok(User = {Name, _Pwd}, State) ->
  lists:member(User, State#state.customers) andalso logged_in(Name, State).


deposit(Name, Pwd, Account, Amount) ->
  gen_server:call({global, bank}, {deposit, {Name, Pwd}, Account, Amount}).
deposit(Name, Pwd, Account, Amount, State) ->
  User = {Name, Pwd},
  case pwd_ok(User, State) of
    true -> case lists:keyfind({Account, Name}, 1, State#state.accounts) of
              OldAcc = {N, Balance} -> NewBalance = Balance + Amount,
                                       {State#state{accounts =
                                                    (State#state.accounts -- [OldAcc]) ++
                                                      [{N, NewBalance}]}, ok};
              _                     -> {State, false}
            end;
    false -> {State, false}
  end.

withdraw(Name, Pwd, Account, Amount) ->
  gen_server:call({global, bank}, {withdraw, {Name, Pwd}, Account, Amount}).
withdraw(Name, Pwd, Account, Amount, State) ->
  User = {Name, Pwd},
  case pwd_ok(User, State) of
    true -> case lists:keyfind({Account, Name}, 1, State#state.accounts) of
              OldAcc = {N, Balance} when Balance >= Amount ->
                NewBalance = Balance - Amount,
                {State#state{accounts = (State#state.accounts -- [OldAcc]) ++ [{N, NewBalance}]}, ok};
              _ -> {State, false}
            end;
    false -> {State, false}
  end.


