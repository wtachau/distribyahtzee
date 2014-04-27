%% @author Will Tachau, James Reinke
%%
%% Usage:
%% erl -compile yahtzee
%% erl -noshell -run yahtzee main tm1 -run init stop -noshell
%%

-module(yahtzee).

%% ====================================================================
%%   Export functions
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

	listen([], []).



%% ====================================================================
%%   Helper functions
%% ====================================================================	
	
timestamp() ->
    {_, _, Micros} = now(), 
    {{_, _, _}, {Hour, Min, Sec}} = calendar:now_to_local_time(now()),
    lists:concat([Hour, ":", Min, ":", Sec, ".", Micros]).



%% ====================================================================
%%   Listen function
%% ====================================================================	

% Continuous listening method
listen(Players, StartRequests)->

	% handle requests, starting new tournament if necessary
	% NewStartRequests = handle_request(Players, StartRequests),
	NewStartRequests = StartRequests,

	io:format("~p Requests: ~p~n",[timestamp(), NewStartRequests]),

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
			listen(NewPlayers, NewStartRequests);

		{request_tournament, PID, {NumberPlayers, GamesPerMatch}} ->
			io:format("~p Received start_tournament message {~p,~p} from ~p, currently ~p players~n", [timestamp(), NumberPlayers, GamesPerMatch, PID, length(Players)]),
			if
				NumberPlayers > length(Players) ->
					io:format("~p Too few players... going to wait~n", [timestamp()]);
				true ->
					io:format("~p Enough players! Starting tournament...~n", [timestamp()])
			end,
			listen(Players, NewStartRequests ++ [{PID, NumberPlayers, GamesPerMatch}]);			

		{'DOWN', MonitorReference, process, PID, Reason} ->
			io:format("~p Process ~p died because ~p! Logging out ~p...~n", [timestamp(), PID, Reason, MonitorReference]),

			NewPlayers = logoutPlayer(Players, PID),

			io:format("~p All Players: ~p~n", [timestamp(), NewPlayers]),

			listen(NewPlayers, NewStartRequests);

		{__, PID, __} ->
			io:format("~p Received unknown message from ~p~n", [timestamp(), PID]),
			listen(Players, NewStartRequests)
	end.

%% ====================================================================
%%   Login/Logout functions
%% ====================================================================	

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

%% ====================================================================
%%   Start Tournament functions
%% ====================================================================	

handle_request(__, []) ->
	[];
handle_request(Players, Requests) ->
	{PID, NumberPlayers, __} = hd(Requests),

	if
		length(Players) >= NumberPlayers  ->
			io:format("~p Starting tournament requested by ~p~n", [timestamp(), PID]),

			% Start a tournament!
			start_tournament(Players, hd(Requests)),

			% and recurse without this request
			handle_request(Players, tl(Requests));
		true ->
			% keep recursing
			[hd(Requests)] ++ handle_request(Players, tl(Requests))
	end.


start_tournament(Players, {PID, NumberPlayers, GamesPerMatch}) ->
	%% this makes bracket and sends spawn messages, deletes request

	TwoPlayers = {hd(Players), hd(tl(Players))}, %FIXME - make bracket
	TID = make_ref(), % FIXME - should be integer?

	% Spawn a match process
	spawn(yahtzee_mm, play_match, [TwoPlayers, GamesPerMatch, TID]).





