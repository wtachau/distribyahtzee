%% @author Will Tachau, James Reinke
%%
%% Usage:
%% erl -compile yahtzee
%% erl -noshell -run yahtzee main tm1 -run init stop -noshell
%%

-module(yahtzee).

%% ====================================================================
%% API functions
%% ====================================================================
-export([main/1]).

main(Params) ->

	% The name to start the network kernel with, globally
	Name = hd(Params),
	
	%% IMPORTANT: Start the empd daemon!
	os:cmd("epmd -daemon"),
	net_kernel:start([list_to_atom(Name), shortnames]),
	register(yahtzee, self()),
	io:format("~p Registered as node ~p, name ~p, nodes ~p~n", [timestamp(), node(), Name, nodes()]),

	listen([]).

%% ====================================================================
%% Internal functions
%% ===========================`=========================================	
	
timestamp() ->
    {_, _, Micros} = now(), 
    {{_, _, _}, {Hour, Min, Sec}} = calendar:now_to_local_time(now()),
    lists:concat([Hour, ":", Min, ":", Sec, ".", Micros]).

% Continuous listening method
listen(Players)->
	receive
		{login, PID, {Username, Password}} ->
			io:format("~p Received login from ~p at ~p~n",[timestamp(), Username, PID]),

			% create login ticket for player, login, and send conf
			LoginTicket = make_ref(),
			{NewPlayers, Success} = loginPlayer(Players, Username, Password, PID, LoginTicket),
			if
				Success == yes ->
					% monitor it, and confirm
					erlang:monitor(process, PID),
					PID ! {logged_in, self(), LoginTicket};
				true ->
					io:format("~p BAD PASSWORD: User ~p's pw doesn't match~n", [timestamp(), Username]),
					PID ! {bad_login, self(), none}
			end,

			io:format("~p All Players: ~p~n", [timestamp(), NewPlayers]),

			% Remember player
			listen(NewPlayers);

		%{start_tournament, PID, NumberPlayers} ->
		%
		%	% set flag to YES, remember number somehow
		%	ok,

		{'DOWN', MonitorReference, process, PID, Reason} ->
			io:format("~p Process ~p died! Logging out...~n", [timestamp(), PID]),

			NewPlayers = logoutPlayer(Players, PID),

			io:format("~p All Players: ~p~n", [timestamp(), NewPlayers]),

			listen(NewPlayers);

		{__, PID, __} ->
			io:format("~p Received unknown message from ~p~n", [timestamp(), PID]),
			listen(none)
	end.

% Add a player to the list, if not already there
loginPlayer([], Username, Password, PID, LoginTicket) ->
	{[{Username, Password, PID, LoginTicket}], yes};
loginPlayer(Players, Username, Password, PID, LoginTicket) ->
	{U, P, _, _} = hd(Players),
	if
		U == Username ->
			% check password
			if 
				P == Password ->
					io:format("~p Logging back in player ~p~n", [timestamp(), U]),
					AltPlayers = Players -- [hd(Players)],
					{AltPlayers ++ [{U, P, PID, LoginTicket}], yes};
				true -> %pw doesn't match
					{Players, no}
			end;
		true ->
			{OtherPlayers, Success} = loginPlayer(tl(Players), Username, Password, PID, LoginTicket),
			{[hd(Players)]++ OtherPlayers, Success}
	end.

% Make a player's PID and LoginTicket none, effectively logging out
logoutPlayer([], PID) ->
	io:format("~p ERROR: Logout Player with PID ~p not found!~n", [timestamp(), PID]);
logoutPlayer(Players, PID) ->
	{U, Pw, Pd, _} = hd(Players),
	if
		PID == Pd ->
			io:format("~p Logging out player~p~n",[timestamp(), U]),
			AltPlayers = Players -- [hd(Players)],
			AltPlayers ++ [{U, Pw, none, none}];
		true ->
			[hd(Players)] ++ logoutPlayer(tl(Players), PID)
	end.








