-module(bank).

%              {bool(), [{name(), acc_name(), int()}], [{name(), pwd()}]}
-record(state, {open = false, accounts = [], customers = []}).

-export([launch/0,stop/0,
         init/1,terminate/2,handle_call/3, handle_cast/2, handle_info/2,code_change/3,
         open/0, close/0, create_user/1, create_account/2]).

-behaviour(gen_server).

launch() ->
  gen_server:start({global, bank}, bank, [], []).

stop() ->
  gen_server:stop({global, bank}).

init(_Args) ->
  {ok, #state{}}.

terminate(_Reason, _State) ->
  ok.

handle_call(Req, _From, State) ->
  {NewState, Res} = case Req of
    close         -> {State#state{open = false}, ok};
    open         -> {State#state{open = true}, ok};
    {create_user, User} -> create_user(User, State);
    {create_account, AccountName, UserName} -> create_account(AccountName, UserName, State);
    {login, User} -> login(User, State);
    {logout, User} -> logout(User, State);
    {deposit, Dep} -> deposit(Dep, State);
    {withdraw, With} -> withdraw(With, State)
  end,
  {reply, Res, NewState}.

code_change(_,S,_) -> {ok, S}.

handle_cast(Req, State) ->
  {reply, Res, NewState} = handle_call(Req, self(), State),
  {noreply, Res, NewState}.

handle_info(_Info, State) ->
  {noreply, State}.

%bank(open, State) ->
  %receive
    %close -> bank(close, State);

open() -> gen_server:call({global, bank}, open).
close() -> gen_server:call({global, bank}, close).

create_user(User) -> gen_server:call({global, bank}, {create_user, User}).
create_user({Name, Pwd}, State) ->
  %io:format("Cr : ~p~n", [lists:keyfind(Name, 1, State#state.customers)]),
  case lists:keyfind(Name, 1, State#state.customers) of
    false -> {State#state{customers = [{Name, Pwd} | State#state.customers]}, ok};
    _  -> {State, false}
  end.

create_account(AccountName, UserName) ->
  gen_server:call({global, bank}, {create_account, AccountName, UserName}).
create_account(AccountName, UserName, State) ->
  Accounts = State#state.accounts,
  case lists:filter(fun({A, U, _B}) -> A == AccountName andalso U == UserName end, Accounts) of
    false -> {State#state{accounts = [{UserName, AccountName, 0} | Accounts]}, ok};
    _     -> {State, false}
  end.

login(User, State) -> {undefined, false}.
logout(User, State) -> {undefined, false}.
deposit(Dep, State) -> {undefined, false}.
withdraw(With, State) -> {undefined, false}.

%open() ->
  %case whereis(bank) of
    %undefined -> Pid = spawn(fun() ->  bank(open, #state) end),
                 %register(bank, Pid);
    %Pid       -> Pid ! open
  %end.
