-module(bank).

%              {bool(), [{name(), acc_name(), int()}], [{name(), pwd()}], [name()]}
-record(state, {open = false, accounts = [], customers = [], logged_in = []}).

-export([launch/0,stop/0,
         state/0, exists/2, logged_in/2, % For debugging only. TODO
         init/1,terminate/2,handle_call/3, handle_cast/2, handle_info/2,code_change/3,
         open/0, close/0, create_user/1, create_account/2, login/1, logout/1]).

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
    {create_user, User} -> create_user(User, State);
    {create_account, AccountName, UserName} -> create_account(AccountName, UserName, State);
    {login, User} -> login(User, State);
    {logout, Name} -> logout(Name, State);
    {deposit, Dep} -> deposit(Dep, State);
    {withdraw, With} -> withdraw(With, State);
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

create_user(User) -> gen_server:call({global, bank}, {create_user, User}).
create_user({Name, Pwd}, State) ->
  %io:format("Cr : ~p~n", [lists:keyfind(Name, 1, State#state.customers)]),
  case exists(Name, State) of
    false -> {State#state{customers = [{Name, Pwd} | State#state.customers]}, ok};
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
  case logged_in(UserName, State) of
    true -> Accounts = State#state.accounts,
            case lists:filter(fun({A, U, _B}) -> A == AccountName andalso U == UserName end,
                              Accounts) of
              [] -> {State#state{accounts = [{UserName, AccountName, 0} | Accounts]}, ok};
              _  -> {State, false}
            end;
    false -> {State, false}
  end.

logged_in(Name, State) ->
  lists:member(Name, State#state.logged_in).

login(User = {_Name, _Pwd}) ->
  gen_server:call({global, bank}, {login, User}).
login(User = {Name, _Pwd}, State) ->
  case lists:member(User, State#state.customers) of
    false -> {State, false};
    true  -> case logged_in(Name, State) of
               true -> {State, false};
               false -> {State#state{logged_in = [Name | State#state.logged_in]}, ok}
             end
  end.

logout(Name) ->
  gen_server:call({global, bank}, {logout, Name}).
logout(Name, State) ->
  case lists:keyfind(Name, 1, State#state.customers) of
    false -> {State, false};
    _     -> {State#state{logged_in = State#state.logged_in -- [Name]}, ok}
  end.


deposit(Dep, State) -> {undefined, false}.

withdraw(With, State) -> {undefined, false}.
