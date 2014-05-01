%% @author Will Tachau, James Reinke
%%
%% Usage:
%% erl -compile yahtzee
%% erl -noshell -run yahtzee main tm1 -run init stop -noshell
%% {yahtzee_manager, list_to_atom("tm1@<host>")} ! {request_tournament, self(), {2, 3}}.
%%

-module(yahtzee).
-define(TIMEOUT, 3000).

%% ====================================================================
%%   Export functions
%% ====================================================================
-compile(export_all).

main(Params) ->

	% The name to start the network kernel with, globally
	Name = hd(Params),
	
	%% IMPORTANT: Start the empd daemon!
	os:cmd("epmd -daemon"),
	net_kernel:start([list_to_atom(Name), shortnames]),
	register(yahtzee_manager, self()),
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
listen(Players, Tournaments)->

	io:format("~p Listening with Tournaments:~n ~p~n And Players:~n~p~n", [timestamp(), Tournaments, Players]),

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
			listen(NewPlayers, Tournaments);

		{logout, PID, _} ->
			NewPlayers = logoutPlayer(Players, PID),
			listen(NewPlayers, Tournaments);

		{request_tournament, PID, {NumberPlayers, GamesPerMatch}} ->
			io:format("~p Received start_tournament message {~p,~p} from ~p, currently ~p players~n", [timestamp(), NumberPlayers, GamesPerMatch, PID, length(Players)]),
			{TID, Winner, GamesPer, NewBracket} = start_tournament(Players, {PID, NumberPlayers, GamesPerMatch}),
			listen(Players, Tournaments ++ [{TID, Winner, GamesPer, NewBracket}]);			

		{'DOWN', MonitorReference, process, PID, Reason} ->
			io:format("~p Process ~p died because ~p! Logging out ~p...~n", [timestamp(), PID, Reason, MonitorReference]),

			NewPlayers = logoutPlayer(Players, PID),

			io:format("~p All Players: ~p~n", [timestamp(), NewPlayers]),

			listen(NewPlayers, Tournaments);

		{match_result, _, {Winner, TID, MID}} ->
			io:format("~p Got result of match #~p from tournament ~p, ~p won~n", [timestamp(), MID, TID, Winner]),
			% Update players to reflect win count, and update tournaments
			UpdatedTournaments = update_tournaments(Tournaments, MID, TID, Winner),
			UpdatedPlayers = update_players(Players, Winner, match),
			listen(UpdatedPlayers, UpdatedTournaments);

		{tournament_result, _, {Winner, TID}} ->
			io:format("~p Player ~p wins tournament ~p!!~n", [timestamp(), Winner, TID]),
			% tournament will already have been updated by match_result
			UpdatedPlayers = update_players(Players, Winner, tournament),
			listen(UpdatedPlayers, Tournaments);

		{tournament_info, PID, TID} ->
			{Status, Winner, Optional} = get_status(Tournaments, TID),
			io:format("~p Tournament Info requested, returning ~n{~p, ~p, ~p}~n", [timestamp(), Status, Winner, Optional]),
			PID ! {tournament_status, PID, {TID, Status, Winner, Optional}},
			listen(Players, Tournaments);

		{_, PID, _} ->
			io:format("~p Received unknown message from ~p~n", [timestamp(), PID]),
			listen(Players, Tournaments)
	end.

%% ====================================================================
%%   Login/Logout functions
%% ====================================================================	

% Add a player to the list, if not already there
loginPlayer([], Username, Password, PID, LoginTicket) ->
	{[{Username, Password, 0, 0, PID, LoginTicket}], yes};
loginPlayer(Players, Username, Password, PID, LoginTicket) ->
	{U, P, Tw, Mw, _, _} = hd(Players),
	if
		U == Username ->
			% check password
			if 
				P == Password ->
					io:format("~p Logging back in player ~p~n", [timestamp(), U]),
					AltPlayers = Players -- [hd(Players)],
					{AltPlayers ++ [{U, P, Tw, Mw, PID, LoginTicket}], yes};
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
	{U, Pw, Tw, Mw, Pd, _} = hd(Players),
	if
		PID == Pd ->
			io:format("~p Logging out player~p~n",[timestamp(), U]),
			AltPlayers = Players -- [hd(Players)],
			AltPlayers ++ [{U, Pw, Tw, Mw, none, none}];
		true ->
			[hd(Players)] ++ logoutPlayer(tl(Players), PID)
	end.

%% ====================================================================
%%   Start Tournament functions
%% ====================================================================	

start_tournament(Players, {PID, NumberPlayers, GamesPerMatch}) ->
	
	%% this makes bracket and sends spawn messages, deletes request

	{_, Secs, Micros} = now(), 
	TID = Secs * Micros,  % semi-unique TID
	io:format("~p Tournament Manager: Starting tournament #~p requested by ~p~n", [timestamp(), TID, PID]),

	% Figure out how many players in tournament (NumberPlayers may not be of form 2^k)
	Total = get_num_players(NumberPlayers, 1),
	%Levels = log2(Total),

	% Generate list of players, with appropriate # of byes
	PlayersWithByes = get_player_list(Players, Total, TID),
	io:format("~p~n",[PlayersWithByes]),

	Bracket = make_bracket(PlayersWithByes, TID, GamesPerMatch),

	io:format("~p Bracket: ~p~n", [timestamp(), Bracket]),
	{TID, none, GamesPerMatch, Bracket}.

%log2(X) ->
%  math:log(X) / math:log(2).


%% ====================================================================
%%   Functions to make bracket
%% ====================================================================

% Structure a bracket as a list of games, and start first games
make_bracket(AllPlayers, TID, GamesPerMatch) ->
	HalfNumPlayers = round(length(AllPlayers)/2),
	NoneBracket =get_empties(HalfNumPlayers - 1),
	RestBracket = get_tuples(AllPlayers, HalfNumPlayers, TID, GamesPerMatch),
	NoneBracket ++ RestBracket.

% Get unplayed games
get_empties(0) ->
	[];
get_empties(Num) ->
	get_empties(Num-1) ++ [{Num, none, none}].

% Schedule first round. ** Also start matches
get_tuples([], _, _, _) ->
	[];
get_tuples([Player1|Players], Num, TID, GamesPerMatch) ->
	Player2 = hd(Players),
	start_match({Player1, Player2}, GamesPerMatch, TID, Num),
	[{Num, Player1, Player2}] ++ get_tuples(tl(Players), Num+1, TID, GamesPerMatch).



%% ====================================================================
%%   Generate Player List
%% ====================================================================

% Return next highest number of form 2^k
get_num_players(Num, SoFar)->
	if
		SoFar >= Num ->
			SoFar;
		true ->
			get_num_players(Num, SoFar * 2)
	end.

 get_player_list(Players, Total, TID) ->
 	ConfirmedPlayers = get_confirmed_players(Players, Total, TID),
 	io:format("~p Confirmed players: ~p~n", [timestamp(), ConfirmedPlayers]),
 	get_full_list(ConfirmedPlayers, Total).

 
% iterate through Players, call for confirmation
get_confirmed_players([], _, _) ->
	[];
get_confirmed_players(_, 0, _) ->
	[];
get_confirmed_players(Players, Total, TID) ->
	% send conf messages
	{Username1, _, _, _, PID, _} = hd(Players),
	if
		PID == none -> %% user is logged out!
			get_confirmed_players(tl(Players), Total, TID);
		true ->
			PID ! {start_tournament, self(), TID},
			% Receive confirmation
			receive
				{accept_tournament, _, _} ->
					io:format("~p Received accept_tournament confirmation from ~p ~n",[timestamp(), Username1]),
					[hd(Players)] ++ get_confirmed_players(tl(Players), Total-1, TID)
			after ?TIMEOUT -> 
				io:format("~p Timed out waiting for accept_tournament reply from ~p!~n", [timestamp(), Username1]),
				get_confirmed_players(tl(Players), Total, TID)
			end
	end.

 	
% Generate list of players, with byes randomly inserted where necessary 
get_full_list([], 2) ->
	[bye,bye];
get_full_list([Player], 2) ->
	[bye, Player];
get_full_list(Players, 2) ->
	Players;
get_full_list(Players, TotalNum) ->
	LenRealPlayers = length(Players),
	LenFirstHalf = round(LenRealPlayers/2),
	FirstHalfPlayers = lists:sublist(Players, LenFirstHalf),
	SecondHalfPlayers = lists:nthtail(LenFirstHalf, Players),
	FirstHalfList = get_full_list(FirstHalfPlayers, round(TotalNum/2)),
	SecondHalfList = get_full_list(SecondHalfPlayers, round(TotalNum/2)),
	FirstHalfList ++ SecondHalfList.


%% ====================================================================
%%   Start a match!
%% ====================================================================

start_match(TwoPlayers, GamesPerMatch, TID, MID) ->
	% Spawn a match process %fixme = in bracket call
	MM = spawn(yahtzee_mm, main, []),
	MM ! {start_match, self(), {TwoPlayers, GamesPerMatch, TID, MID}}.


%% ====================================================================
%%   Update tournaments, and start other match if necessary
%% ====================================================================

% Find tournament with correct TID, and update
update_tournaments([], _, _, _) ->
	[];
update_tournaments(Tournaments, MID, TID, Winner) ->
	{T, W, G, B} = hd(Tournaments),
	if
		T == TID ->
			{UpdatedBracket, NewWinner} = update_tournament(B, MID, Winner, G, T),
			[{T, NewWinner, G, UpdatedBracket}] ++ tl(Tournaments);
		true ->
			[{T, W, G, B}] ++ update_tournaments(tl(Tournaments), MID, TID, Winner)
	end.

% Update bracket with winner, start next match if appropriate
update_tournament(Bracket, 1, Winner, _, _) ->
	{Bracket, Winner};
update_tournament(Bracket, MID, Winner, GamesPerMatch, TID) ->
	NextRoundNum = round((MID-1)/2),
	NextRound = lists:nth(NextRoundNum, Bracket),
	UpdatedRound = update_round(NextRound, Winner, GamesPerMatch, TID),
	{lists:sublist(Bracket,NextRoundNum-1) ++ [UpdatedRound] ++ lists:nthtail(NextRoundNum,Bracket), none}.

% Add players to a match. If two, start the match!
update_round({Num, none, none}, Player1, _, _) ->
	{Num, Player1, none};
update_round({Num, Player1, none}, Player2, GamesPerMatch, TID) ->
	% start the match
	start_match({Player1, Player2}, GamesPerMatch, TID, Num),
	{Num, Player1, Player2}.

%% ====================================================================
%%   Update players for a Tournament Win (TW) or Match Win (MW)
%% ====================================================================

update_players(Players, bye, _) ->
	Players;
update_players([], _, _) ->
	[];
update_players([bye|Players], Winner, Kind) ->
	[bye] ++ update_players(Players, Winner, Kind);
update_players(Players, Winner, Kind) ->
	{Username, Pw, Tw, Mw, Pd, Lt} = hd(Players),
	{U, _, _, _, _, _} = Winner,
	if
		U == Username ->
		if
			Kind == tournament ->
				[{Username, Pw, (Tw+1), Mw, Pd, Lt}] ++ tl(Players);
			true ->
				[{Username, Pw, Tw, (Mw+1), Pd, Lt}] ++ tl(Players)
		end;
		true ->
			[{Username, Pw, Tw, Mw, Pd, Lt}] ++ update_players(tl(Players), Winner, Kind)
	end.


%% ====================================================================
%%   Return information about a tournament
%% ====================================================================

get_status([], _) ->
	%invalid TID, ignore
	{none, none, none}; 
get_status(Tournaments, TID) ->
	{T, W, _, B} = hd(Tournaments),
	if
		T == TID ->
			if
				W == none ->
					{in_progress, undefined, B};
				true ->
					{complete, W, B}
			end;
		true ->
			get_status(tl(Tournaments), TID)
	end.



