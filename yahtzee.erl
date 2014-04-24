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

			% monitor it
			erlang:monitor(process, PID),

			% create login ticket for player
			LoginTicket = make_ref(),
			PID ! {logged_in, self(), LoginTicket},

			NewPlayers = loginPlayer(Players, Username, PID, LoginTicket),

			io:format("~p All Players: ~p~n", [timestamp(), NewPlayers]),

			% Remember player
			listen(NewPlayers);

		%{start_tournament, PID, NumberPlayers} ->
		%
		%	% set flag to YES, remember number somehow
		%	ok,

		{'DOWN', MonitorReference, process, PID, Reason} ->
		%{'DOWN', __, process, PID, __} ->
			io:format("~p Process ~p died! Logging out...~n", [timestamp(), PID]),

			NewPlayers = logoutPlayer(Players, PID),

			io:format("~p All Players: ~p~n", [timestamp(), NewPlayers]),

			listen(NewPlayers);

		{__, PID, __} ->
			io:format("~p Received unknown message from ~p~n", [timestamp(), PID]),
			listen(none)
	end.

% Add a player to the list, if not already there
loginPlayer([], Username, PID, LoginTicket) ->
	[{Username, PID, LoginTicket}];
loginPlayer(Players, Username, PID, LoginTicket) ->
	{U, _, _} = hd(Players),
	if
		U == Username ->
			io:format("~p Logging back in player ~p~n", [timestamp(), U]),
			AltPlayers = Players -- [hd(Players)],
			AltPlayers ++ [{U, PID, LoginTicket}];
		true ->
			[hd(Players)]++loginPlayer(tl(Players), Username, PID, LoginTicket)
	end.

logoutPlayer([], PID) ->
	io:format("~p ERROR: Logout Player with PID ~p not found!~n", [timestamp(), PID]);
logoutPlayer(Players, PID) ->
	{U, P, _} = hd(Players),
	if
		PID == P ->
			io:format("~p Logging out player~p~n",[timestamp(), U]),
			AltPlayers = Players -- [hd(Players)],
			AltPlayers ++ [{U, none, none}];
		true ->
			[hd(Players)] ++ logoutPlayer(tl(Players), PID)
	end.








