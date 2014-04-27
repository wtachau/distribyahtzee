%% @author Will Tachau, James Reinke
%%
%% {yahtzee, list_to_atom("tm1@William-Tachaus-MacBook-Pro-2")} ! {request_tournament, self(), something}.

-module(yahtzee_mm).
-define(TIMEOUT, 3000).

%% ====================================================================
%%   Main function
%% ====================================================================
-compile(export_all).

play_match({Player1, Player2}, NumGames, TID) ->
	{Username1, __, PID1, __} = Player1,
	{Username2, __, PID2, __} = Player2,

	% send conf messages
	PID1 ! {start_tournament, self(), TID},
	% Receive confirmation
	receive
		{accept_tournament, P1, __} ->
			io:format("~p Received accept_tournament confirmation from ~p ~n",[timestamp(), P1])
	after ?TIMEOUT -> 
		io:format("Timed out waiting for accept_tournament reply from ~p!~n", [PID1])
	end,

	% send conf messages
	PID2 ! {start_tournament, self(), TID},
	% Receive confirmation
	receive
		{accept_tournament, P2, __} ->
			io:format("~p Received accept_tournament confirmation from ~p ~n",[timestamp(), P2])
	after ?TIMEOUT -> 
		io:format("Timed out waiting for accept_tournament reply from ~p!~n", [PID2])
	end,

	io:format("~p Match Manager starting with ~p vs ~p, ~p games~n", [timestamp(), Username1, Username2, NumGames]),

	% Now start games!
	{Score1, Score2} = play_games(Player1, Player2, NumGames, TID).

%% ====================================================================
%%   Internal functions
%% ====================================================================	
	
timestamp() ->
    {_, _, Micros} = now(), 
    {{_, _, _}, {Hour, Min, Sec}} = calendar:now_to_local_time(now()),
    lists:concat([Hour, ":", Min, ":", Sec, ".", Micros]).



%% ====================================================================
%%   Function to Play the Games
%% ====================================================================	

play_games(__, __, 0, __) ->
	{0, 0};
play_games(Player1, Player2, NumGames, TID) ->

	% Set up scorecards
	S1 = [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0],
	S2 = [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0],
	GID = make_ref(), %FIXME - what kind of id?
	% Return score + 1 for the winner

	{Scorecard1, Scorecard2} = play_a_game(Player1, Player2, S1, S2, 0, GID, TID),

	% total up scorecards
	Sum1 = total(Scorecard1),
	Sum2 = total(Scorecard2),

	% And Recurse
	{Score1, Score2} = play_games(Player1, Player2, NumGames-1, TID),

	if
		Sum1 > Sum2 ->
			{Score1 + 1, Score2};
		Sum2 > Sum1 ->
			{Score1, Score2 + 1};
		true -> % FIXME: TIE
			{Score1, Score2}
	end.


play_a_game(__, __, Scorecard1, Scorecard2, 13, __, __) ->
	{Scorecard1, Scorecard2};
play_a_game(Player1, Player2, Scorecard1, Scorecard2, TurnNumber, GID, TID) ->

	% FIXME: Generate Dice randomly
	AllDice = [[1,2,3,4,5], [1,2,3,4,5], [1,2,3,4,5]],

	% get turn from player 1, then player 2
	NewScorecard1 = handle_turn(Player1, TurnNumber, 1, Scorecard1, Scorecard2, GID, TID, AllDice),
	NewScorecard2 = handle_turn(Player2, TurnNumber, 1, Scorecard2, Scorecard1, GID, TID, AllDice),

	{NewScorecard1, NewScorecard2}.

% Handle a turn
handle_turn(__, __, 4, Scorecard, __, __, __, __) ->
	Scorecard;
handle_turn(Player, TurnNumber, RollNumber, Scorecard, OppScorecard, GID, TID, AllDice) ->

	DiceList = lists:nth(RollNumber, AllDice),

	{Username, __, PID, __} = Player,
	Ref = make_ref(),
	% send message to player
	PID ! {play_request, self(), {Ref, TID, GID, TurnNumber, RollNumber, DiceList, Scorecard, OppScorecard}},
	receive
		{play_action, P1, {RRef, RTID, RGID, RRollNumber, DiceToKeep, ScorecardLine}} ->

			io:format("~p Received play_action message from ~p ~n",[timestamp(), Username])

			% Now do something with response!

	after ?TIMEOUT -> 
		io:format("Timed out waiting for play_action reply from ~p!~n", [Username])
	end.

total(Scorecard) ->
	0.








