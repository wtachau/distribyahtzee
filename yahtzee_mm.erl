%% @author Will Tachau, James Reinke
%%
%% {yahtzee, list_to_atom("tm1@William-Tachaus-MacBook-Pro-2")} ! {request_tournament, self(), something}.

-module(yahtzee_mm).
-define(TIMEOUT, 3000).

%% ====================================================================
%%   Main function
%% ====================================================================
-compile(export_all).

main() ->
	% Listen for tournament manager (so can get return info)
	receive
		{start_match, PID, {{Player1, Player2}, NumGames, TID}} ->
			io:format("~p (MatchManager:) Received start_match confirmation from ~p ~n",[timestamp(), PID]),
		play_match({Player1, Player2}, NumGames, TID, PID)

	after ?TIMEOUT -> 
		io:format("~p (MatchManager:) Timed out waiting for start_match message~n", [timestamp()])
	end.

play_match({Player1, Player2}, NumGames, TID, TournamentPID) ->
	{Username1, _, PID1, _} = Player1,
	{Username2, _, PID2, _} = Player2,

	%FIXME - these should go in tournament manager!

	io:format("~p (MatchManager:) Sending start_tournament messages to ~p and ~p~n", [timestamp(), Username1, Username2]),

	% send conf messages
	PID1 ! {start_tournament, self(), TID},
	% Receive confirmation
	receive
		{accept_tournament, P1, _} ->
			io:format("~p (MatchManager:) Received accept_tournament confirmation from ~p ~n",[timestamp(), P1])
	after ?TIMEOUT -> 
		io:format("~p (MatchManager:) Timed out waiting for accept_tournament reply from ~p!~n", [timestamp(), PID1])
	end,

	% send conf messages
	PID2 ! {start_tournament, self(), TID},
	% Receive confirmation
	receive
		{accept_tournament, P2, _} ->
			io:format("~p (MatchManager:) Received accept_tournament confirmation from ~p ~n",[timestamp(), P2])
	after ?TIMEOUT -> 
		io:format("~p (MatchManager:) Timed out waiting for accept_tournament reply from ~p!~n", [timestamp(), PID2])
	end,

	io:format("~p (MatchManager:) Starting with ~p vs ~p, ~p games~n", [timestamp(), Username1, Username2, NumGames]),

	% Now start games!
	{Score1, Score2} = play_games(Player1, Player2, NumGames, NumGames, TID),

	io:format("~p (MatchManager:) *!*!* Final Score--> Player ~p:~p, Player ~p:~p~n", [timestamp(), Username1, Score1, Username2, Score2]),

	% Determine winner and send back to tournament manager
	if
		Score1 > Score2 ->
			io:format("~p (MatchManager:) Player ~p wins!~n", [timestamp(), Username1]),
			TournamentPID ! {tournament_result, self(), {Player1, TID}};
		true ->
			io:format("~p (MatchManager:) Player ~p wins!~n", [timestamp(), Username2]),
			TournamentPID ! {tournament_result, self(), {Player2, TID}}
	end.

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

% Play the number of games specified. Keep track of score, send each game to play_a_game
play_games(_, _, 0, _, _) ->
	{0, 0};
play_games(Player1, Player2, NumGamesLeft, NumGamesTotal, TID) ->

	{Username1, _, _, _} = Player1,
	{Username2, _, _, _} = Player2,
	io:format("~p (MatchManager:) Starting Game ~p between ~p and ~p, ~p games left~n", [timestamp(), (NumGamesTotal-NumGamesLeft), Username1, Username2, NumGamesLeft]),

	% Set up scorecards
	S1 = [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0],
	S2 = [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0],
	GID = make_ref(), %FIXME - what kind of id?
	
	% Get final scorecards after a game
	{Scorecard1, Scorecard2} = play_a_game(Player1, Player2, S1, S2, 0, GID, TID),

	% total up scorecards
	Sum1 = total(Scorecard1),
	Sum2 = total(Scorecard2),

	io:format("~p (MatchManager:) *!* Game ~p: ~p got ~p, ~p got ~p~n", [timestamp(), (NumGamesTotal-NumGamesLeft), Username1, Sum1, Username2, Sum2]),

	% And Recurse
	{Score1, Score2} = play_games(Player1, Player2, NumGamesLeft-1, NumGamesTotal, TID),

	% Return score + 1 for the winner
	if
		Sum1 > Sum2 ->
			{Score1 + 1, Score2};
		Sum2 > Sum1 ->
			{Score1, Score2 + 1};
		true -> % FIXME: TIE
			{Score1, Score2}
	end.


% Play one game of yahtzee. Call handle_turn for each turn, call itself recursively for each round
play_a_game(_, _, Scorecard1, Scorecard2, 13, _, _) ->
	{Scorecard1, Scorecard2};
play_a_game(Player1, Player2, Scorecard1, Scorecard2, TurnNumber, GID, TID) ->

	% FIXME: Generate Dice randomly
	AllDice = [[1,2,3,4,5], [6,7,8,9,10], [11,12,13,14,15]],

	io:format(">> Dice rolled for turn ~p: ~p~n", [TurnNumber, AllDice]), %fixme: take out

	% used in handle_turn to keep track of which dice are kept
	MyDice = [0,0,0,0,0], 
	% get turn from player 1, then player 2
	NewScorecard1 = handle_turn(Player1, TurnNumber, 1, Scorecard1, Scorecard2, GID, TID, AllDice, MyDice),
	NewScorecard2 = handle_turn(Player2, TurnNumber, 1, Scorecard2, Scorecard1, GID, TID, AllDice, MyDice),

	play_a_game(Player1, Player2, NewScorecard1, NewScorecard2, TurnNumber + 1, GID, TID).


% Handle a turn. 
handle_turn(_, _, 4, Scorecard, _, _, _, _, _) ->
	Scorecard;
handle_turn(Player, TurnNumber, RollNumber, Scorecard, OppScorecard, GID, TID, AllDice, KeptDice) ->

	DiceList = lists:nth(RollNumber, AllDice),

	{Username, _, PID, _} = Player,
	Ref = make_ref(),
	% send message to player
	io:format("~p (MatchManager:) Sending play_request to ~p on turn #~p, with dice ~p~n", [timestamp(), Username, TurnNumber, DiceList]),
	PID ! {play_request, self(), {Ref, TID, GID, TurnNumber, RollNumber, DiceList, Scorecard, OppScorecard}},
	
	receive
		{play_action, P1, {RRef, RTID, RGID, RRollNumber, DiceToKeep, ScorecardLine}} ->

			io:format("~p (MatchManager:) Received play_action message from ~p ~n",[timestamp(), Username]),

			if
				ScorecardLine > 0 ->

					IsLegalMove = isLegal(Scorecard, ScorecardLine, DiceToKeep, KeptDice),
					if
						IsLegalMove == true ->
							% FIXME: determine what value goes in that scorecard spot
							Value = 5,

							% Update scorecard accordingly
							NewScorecard = lists:sublist(Scorecard,ScorecardLine-1) ++ [Value] ++ lists:nthtail(ScorecardLine,Scorecard),

							io:format("~p (MatchManager:) Player ~p ending turn with Scorecard = ~p~n", [timestamp(), Username, NewScorecard]),
							NewScorecard;
						true -> %FIXME cheating!
							io:format("~p (MatchManager:) ERROR: Not a legal move from ~p~n", [timestamp(), Username]),
							Scorecard
					end;
					
				true ->
					if
						RollNumber == 3 ->
							ok;
							%FIXME: violation of the protocol, what to do?
						true ->
							% make mydice depending on dicetokeep
							NewKeptDice = getNewDice(DiceList, KeptDice, DiceToKeep),
							io:format("~p (MatchManager:) Player ~p keeping dice ~p~n", [timestamp(), Username, NewKeptDice]),
							handle_turn(Player, TurnNumber, RollNumber+1, Scorecard, OppScorecard, GID, TID, AllDice, NewKeptDice)
					end	
			end

	after ?TIMEOUT -> 
		io:format("(MatchManager:) Timed out waiting for play_action reply from ~p!~n", [Username])
	end.

% Given a scorecard, return proper sum
total(Scorecard) ->
	0. %fixme


% Given the new dice, dice already kept, and a list of booleans, generate new kept dice
getNewDice([], _, _) ->
	[];
getNewDice([FNewRoll|RNewRoll], [FDiceKept|RDiceKept], [FKeep|RKeep]) ->
	if
		FKeep == true ->
			[FNewRoll] ++ getNewDice(RNewRoll, RDiceKept, RKeep);
		true ->
			[FDiceKept] ++ getNewDice(RNewRoll, RDiceKept, RKeep)
	end.

% Determine if a move is legal
isLegal(Scorecard, ScorecardLine, DiceToKeep, KeptDice) ->
	% is that scorecard line already full?
	ChosenSpotVal = lists:nth(ScorecardLine, Scorecard),
	if
		ChosenSpotVal > 0 ->
			io:format("~p (MatchManager:) ERROR: Scorecard spot ~p already taken~n", [timestamp(), ScorecardLine]),
			false;
		true ->
			true
	end.







