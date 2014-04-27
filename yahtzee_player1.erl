%% @author Will Tachau, James Reinke
%%
%% Usage:
%% erl -compile yahtzee_player1
%% erl -noshell -run yahtzee_player1 main Will u p tm1@<host> tm2@<host> -run init stop -noshell
%%

-module(yahtzee_player1).
-define(TIMEOUT, 3000).

%% ====================================================================
%%   Main function
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
	register(yahtzee, self()),
	% Connect to registry of first tournament manager
	io:format("~p Registered as node ~p, with ~p~n", [timestamp(), node(), nodes()]),

	% login to tournament managers
	login(TManager, Username, Password, []).

%% ====================================================================
%%   Internal functions
%% ===========================`=========================================	
	
timestamp() ->
    {_, _, Micros} = now(), 
    {{_, _, _}, {Hour, Min, Sec}} = calendar:now_to_local_time(now()),
    lists:concat([Hour, ":", Min, ":", Sec, ".", Micros]).

login([], _, _, Connections) ->
	listen(Connections);
login(TManagers, Username, Password, Connections) ->

	% Get a Tournament Manager and send it login message
	TManager = list_to_atom(hd(TManagers)),
	Msg = {login, self(), {Username, Password}},
	io:format("~p Trying to login to ~p~n", [timestamp(), TManager]),
	{yahtzee, TManager} ! Msg,

	% Receive confirmation
	receive
		{logged_in, PID, LoginTicket} ->
			io:format("~p Received logged-in confirmation from ~p with ticket ~p~n",[timestamp(), PID, LoginTicket]),
			login(tl(TManagers), Username, Password, Connections++[{TManager, PID, LoginTicket}]);
		{bad_login, __, __} ->
			io:format("~p Password fail for ~p...~n",[timestamp(), Username])
	after ?TIMEOUT -> 
		io:format("Timed out waiting for logged_in reply from ~p!~n", [TManager]),
		login(tl(TManagers), Username, Password, Connections)
	end.
	

% Continuous listening method
listen(Connections)->
	io:format("~p listening for messages with connections=~p~n",[timestamp(), Connections]),
	receive
		{start_tournament, PID, TID} ->
			io:format("~p Received start_tournament request from ~p with TID ~p~n",[timestamp(), PID, TID]),

			%% FIXME: something about accepting tournament (data structures? does it matter?)

			PID ! {accept_tournament, self(), TID},
			listen(Connections);

		{play_request, PID, {Ref, TID, GID, TurnNumber, RollNumber, DiceList, Scorecard, OppScorecard}} ->
			io:format("~p Received play_request from ~p~n", [timestamp(), PID]),

			%FIXME: how do we come up with DiceToKeep, ScorecardLine?
			
			PID ! {play_action, self(), {Ref, TID, GID, RollNumber, DiceList, 4}},

			listen(Connections);

		{__, __, __} ->
			io:format("~p Received unknown message~n", [timestamp()]),
			listen(none)
	end.






