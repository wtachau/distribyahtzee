%% @author Will Tachau, James Reinke
%%
%% Usage:
%% erl -compile yahtzee_player1
%% erl -noshell -run yahtzee_player1 main networkname -run init stop -noshell
%%

-module(yahtzee_player1).

%% ====================================================================
%% API functions
%% ====================================================================
-export([main/1]).

main(Params) ->

	% The name to start the network kernel with, globally
	Name = hd(Params),

	% Username/Password to use with tournament managers
	Username = hd(tl(Params)),
	Password = hd(tl(tl(Params))),

	TManager = tl(tl(tl(Params))),
	
	%% IMPORTANT: Start the empd daemon!
	os:cmd("epmd -daemon"),
	net_kernel:start([list_to_atom(Name), shortnames]),
	% Connect to registry of first tournament manager
	net_kernel:connect_node(list_to_atom(hd(TManager))),
	timer:sleep(1000),
	io:format("~p Registered as node ~p, with ~p~n", [timestamp(), node(), nodes()]),
	io:format("Connected to ~p~n",[global:registered_names()]),



	% login to tournament managers
	login(TManager, Username, Password),

	listen().

%% ====================================================================
%% Internal functions
%% ===========================`=========================================	
	
timestamp() ->
    {_, _, Micros} = now(), 
    {{_, _, _}, {Hour, Min, Sec}} = calendar:now_to_local_time(now()),
    lists:concat([Hour, ":", Min, ":", Sec, ".", Micros]).

login([], _, _) ->
	ok;
login(TManagers, Username, Password) ->
	TManager = list_to_atom(hd(TManagers)),
	Msg = {login, self(), something},
	io:format("Trying to login to ~p~n", [TManager]),
	global:send(TManager, Msg),
	login(tl(TManagers), Username, Password).

% Continuous listening method
listen()->
	receive
		{logged_in, PID, Data} ->
			io:format("~p Received logged-in from ~p~n",[timestamp(), PID]),
			listen();
		{__, Pid, Data} ->
			io:format("~p Received unknown message~n", [timestamp()]),
			listen
	end.