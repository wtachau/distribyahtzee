%% @author Will Tachau, James Reinke
%%
%% {yahtzee, list_to_atom("tm1@William-Tachaus-MacBook-Pro-2")} ! {request_tournament, self(), something}.

-module(yahtzee_mm).
-define(TIMEOUT, 3000).

%% ====================================================================
%%   Main function
%% ====================================================================
-compile(export_all).
-import(yahtzee_lib, [get_total_of/2, get_total/1, get_score_for/2, in_small_straight/2, in_straight/1, get_min/2, get_max/2, get_most_common/1, most_common/2, get_frequency/1, add_freq/2]).

main() ->
	% Listen for tournament manager (so can get return info)
	receive
		{start_match, PID, {{Player1, Player2}, NumGames, TID, MID}} ->
			io:format("~p (MatchManager:) Received start_match message from Tournament Manager ~n",[timestamp()]),
		play_match({Player1, Player2}, NumGames, TID, PID, MID)

	after ?TIMEOUT -> 
		io:format("~p (MatchManager:) Timed out waiting for start_match message~n", [timestamp()])
	end.

% Play a match as determined by yahztee tournament manager
play_match({bye, bye}, _, TID, TournamentPID, MID) ->
	io:format("~p (MatchManager:) Bye wins against bye!~n", [timestamp()]),
	TournamentPID ! {match_result, self(), {bye, bye, TID, MID}};

play_match({bye, Player2}, _, TID, TournamentPID, MID) ->
	{Username, _, _, _, _, _, _, _} = Player2,
	io:format("~p (MatchManager:) Player ~p wins against bye!~n", [timestamp(), Username]),
	TournamentPID ! {match_result, self(), {Player2, bye, TID, MID}},
	if
		MID == 1 ->
			io:format("~p (MatchManager:) Player ~p wins tournament!~n", [timestamp(), Username]),
			TournamentPID ! {tournament_result, self(), {Player2, TID}};
		true ->
			ok
	end;

play_match({Player1, bye}, _, TID, TournamentPID, MID) ->
	{Username, _, _, _, _, _, _, _} = Player1,
	io:format("~p (MatchManager:) Player ~p wins against bye!~n", [timestamp(), Username]),
	TournamentPID ! {match_result, self(), {Player1, bye, TID, MID}},
	if
		MID == 1 ->
			io:format("~p (MatchManager:) Player ~p wins tournament!~n", [timestamp(), Username]),
			TournamentPID ! {tournament_result, self(), {Player1, TID}};
		true ->
			ok
	end;

play_match({Player1, Player2}, NumGames, TID, TournamentPID, MID) ->
	{Username1, _, _, _, _, _, _, _} = Player1,
	{Username2, _, _, _, _, _, _, _} = Player2,

	io:format("~p (MatchManager:) Starting with ~p vs ~p, ~p games~n", [timestamp(), Username1, Username2, NumGames]),

	% Now start games!
	{Score1, Score2} = play_games(Player1, Player2, NumGames, NumGames, TID, 0),

	io:format("~n~n~p (MatchManager:) *!*!* Final Score--> Player ~p:~p, Player ~p:~p~n", [timestamp(), Username1, Score1, Username2, Score2]),

	% Determine winner and send back to tournament manager
	if
		Score1 > Score2 ->
			io:format("~p (MatchManager:) Player ~p wins!~n", [timestamp(), Username1]),
			TournamentPID ! {match_result, self(), {Player1, Player2, TID, MID}};
		Score1 < Score2 ->
			io:format("~p (MatchManager:) Player ~p wins!~n", [timestamp(), Username2]),
			TournamentPID ! {match_result, self(), {Player2, Player1, TID, MID}};
		true ->
			io:format("~p (MatchManager:) Bye wins!~n", [timestamp()]),
			TournamentPID ! {match_result, self(), {bye, bye, TID, MID}}
	end,
	% And let it know if player won overall tournament
	if
		MID == 1 ->
			if
				Score1 > Score2 ->
					io:format("~p (MatchManager:) Player ~p wins tournament!~n", [timestamp(), Username1]),
					TournamentPID ! {tournament_result, self(), {Player1, TID}};
				Score1 < Score2 ->
					io:format("~p (MatchManager:) Player ~p wins tournament!~n", [timestamp(), Username2]),
					TournamentPID ! {tournament_result, self(), {Player2, TID}};
				true ->
					io:format("~p (MatchManager:) Bye wins tournament!~n", [timestamp()]),
					TournamentPID ! {tournament_result, self(), {bye, TID}}
			end;
		true ->
			ok
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
play_games(_, _, 0, _, _, _) ->
	{0, 0};
play_games(Player1, Player2, NumGamesLeft, NumGamesTotal, TID, TiesSoFar) ->

	{Username1, _, _, _, _, _, _, _} = Player1,
	{Username2, _, _, _, _, _, _, _} = Player2,
	io:format("~p (MatchManager:) Starting Game ~p between ~p and ~p, ~p games left~n", [timestamp(), (NumGamesTotal-NumGamesLeft), Username1, Username2, NumGamesLeft]),

	% Set up scorecards
	S1 = new_scorecard(),
	S2 = new_scorecard(),
	GID = NumGamesTotal - NumGamesLeft,
	
	% Get final scorecards after a game
	% If we've tied k/2 times, play in standard yahtzee mode
	RegularMode = (TiesSoFar < round(NumGamesTotal/2)),
	{Scorecard1, Scorecard2} = play_a_game(Player1, Player2, S1, S2, 0, GID, TID, RegularMode),

	% total up scorecards
	Sum1 = scorecard_total(Scorecard1),
	Sum2 = scorecard_total(Scorecard2),

	io:format("~p (MatchManager:) *!* Game ~p: ~p got ~p, ~p got ~p~n~n", [timestamp(), (NumGamesTotal-NumGamesLeft), Username1, Sum1, Username2, Sum2]),

	% if both players have crashed. Otherwise, continue
	if
		Sum1 == -13 andalso Sum2 == -13 ->
			{-1,-1};
		% If this was a tie game, re-do
		Sum1 == Sum2 andalso Sum1 > -13 ->
			% And Recurse
			{Score1, Score2} = play_games(Player1, Player2, NumGamesLeft, NumGamesTotal, TID, TiesSoFar + 1),

			% To make sure players in recursed games didn't crash
			if
				Score1 >= 0 andalso Score2 >= 0 ->
					{Score1, Score2};
				true ->
					{-1, -1}
			end;
		true ->
			% And Recurse
			{Score1, Score2} = play_games(Player1, Player2, NumGamesLeft-1, NumGamesTotal, TID, TiesSoFar),

			% To make sure players in recursed games didn't crash
			if
				Score1 >= 0 andalso Score2 >= 0 ->
					% Return score + 1 for the winner
					if
						Sum1 > Sum2 ->
							{Score1 + 1, Score2};
						true ->
							{Score1, Score2 + 1}
					end;
				true ->
					{-1, -1}
			end
	end.


% Play one game of yahtzee. Call handle_turn for each turn, call itself recursively for each round
play_a_game(_, _, Scorecard1, Scorecard2, 13, _, _, _) ->
	{Scorecard1, Scorecard2};
play_a_game(Player1, Player2, Scorecard1, Scorecard2, TurnNumber, GID, TID, RegularMode) ->

	% If we're in regular tournament mode, AllDice2 will be the same as AllDice
	AllDice = get_dice_roll(),
	AllDice2 = get_second_dice(AllDice, RegularMode),

	io:format("~p >> Dice rolled for turn ~p: ~p~n", [timestamp(), TurnNumber, AllDice]),
	if
		RegularMode == false ->
			io:format("~p >> STANDARD MODE: Also dice~p~n", [timestamp(), AllDice2]);
		true ->
			ok
	end,

	% get turn from player 1, then player 2. Check if they are cheating/ disconnected
	{Cheating1, NewScorecard1} = handle_turn(Player1, TurnNumber, 1, Scorecard1, Scorecard2, GID, TID, AllDice, hd(AllDice)),
	{Cheating2, NewScorecard2} = handle_turn(Player2, TurnNumber, 1, Scorecard2, Scorecard1, GID, TID, AllDice2, hd(AllDice)),

	% If a player cheated, return scorecards as is... Cheater will automatically lose
	if
		Cheating1 == false andalso Cheating2 == false ->
			play_a_game(Player1, Player2, NewScorecard1, NewScorecard2, TurnNumber + 1, GID, TID, RegularMode);
		true ->
			{NewScorecard1, NewScorecard2}
	end.

	


% Handle a turn. 
handle_turn(_, _, 4, Scorecard, _, _, _, _, _) ->
	{false, Scorecard};
handle_turn(Player, TurnNumber, RollNumber, Scorecard, OppScorecard, GID, TID, AllDice, DiceAvailable) ->

	{Username, _, _, _, _, _, PID, _} = Player,
	Ref = make_ref(),
	% send message to player
	io:format("~p (MatchManager:) Sending play_request to ~p on turn #~p,~n\t\t\t\t\t\twith dice ~p~n", [timestamp(), Username, TurnNumber, DiceAvailable]),
	PID ! {play_request, self(), {Ref, TID, GID, TurnNumber, RollNumber, DiceAvailable, Scorecard, OppScorecard}},
	
	receive
		% {play_action, P1, {RRef, RTID, RGID, RRollNumber, DiceToKeep, ScorecardLine}} ->
		{play_action, _, {_, _, _, _, DiceToKeep, ScorecardLine}} ->

			%io:format("~p (MatchManager:) Received play_action message from ~p:~n\t\t\t>> Keeping ~p, scorecard #~p ~n",[timestamp(), Username, DiceToKeep, ScorecardLine]),

			if
				ScorecardLine > 0 ->

					% Check that this move is legal. If so, return value. If not, -1
					ValueOfMove = value_move(Scorecard, ScorecardLine, DiceAvailable),
					if
						ValueOfMove >= 0 ->

							% Update scorecard accordingly
							NewScorecard = lists:sublist(Scorecard,ScorecardLine-1) ++ [ValueOfMove] ++ lists:nthtail(ScorecardLine,Scorecard),

							io:format("~p (MatchManager:) Player ~p ending turn with Scorecard line #~p: ~n\t\t\t>> Scorecard = ~p~n", [timestamp(), Username, ScorecardLine, NewScorecard]),
							{false, NewScorecard};

						true -> 
							io:format("~p (MatchManager:) ERROR: Not a legal move from ~p~n", [timestamp(), Username]),
							{true, new_scorecard()}
					end;
					
				true ->
					if
						RollNumber == 3 ->
							io:format("~p (MatchManager:) ERROR: Last roll must return scorecard selection ~p~n", [timestamp(), Username]),
							{true, new_scorecard()};
						true ->
							
							% Generate next dice values available to player
							NextDice = lists:nth(RollNumber+1, AllDice),
							NewDice = getNewDice(NextDice, DiceAvailable, DiceToKeep),
							io:format("~p (MatchManager:) Player ~p keeping dice ~p~n", [timestamp(), Username, NewDice]),
							handle_turn(Player, TurnNumber, RollNumber+1, Scorecard, OppScorecard, GID, TID, AllDice, NewDice)
					end	
			end

	after ?TIMEOUT -> 
		io:format("(MatchManager:) Timed out waiting for play_action reply from ~p!~n", [Username]),
		{true, new_scorecard()}
	end.

new_scorecard() ->
	[-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0].


%% ====================================================================
%%   Helper functions
%% ====================================================================	

scorecard_total(Scorecard) ->
	UpperSection = lists:sublist(Scorecard,6),
	LowerSection = lists:sublist(lists:nthtail(6,Scorecard),7),
	UpperTotal = total(UpperSection),
	LowerTotal = total(LowerSection),
	io:format("Upper: ~p [~p], Lower:~p [~p]~n",[UpperSection, UpperTotal, LowerSection, LowerTotal]),
	if
		UpperTotal >= 63 ->
			UpperTotal + LowerTotal + 35;
		true ->
			UpperTotal + LowerTotal
	end.

% If Regular mode, return same dice. Otherwise return new
get_second_dice(Dice, true) ->
	Dice;
get_second_dice(_, false) ->
	get_dice_roll(). 


% Given a scorecard, return proper sum
total([]) ->
	0;
total([FirstScore|Rest]) ->
	FirstScore + total(Rest).


% Given the new dice, dice already kept, and a list of booleans, generate new kept dice
getNewDice([], _, _) ->
	[];
getNewDice([FNewRoll|RNewRoll], [FDiceKept|RDiceKept], [FKeep|RKeep]) ->
	if
		FKeep == false ->
			[FNewRoll] ++ getNewDice(RNewRoll, RDiceKept, RKeep);
		true ->
			[FDiceKept] ++ getNewDice(RNewRoll, RDiceKept, RKeep)
	end.

% Determine if a move is legal
value_move(Scorecard, ScorecardLine, Dice) ->
	% is that scorecard line already full?
	ChosenSpotVal = lists:nth(ScorecardLine, Scorecard),
	if
		ChosenSpotVal > 0 ->
			io:format("~p (MatchManager:) ERROR: Scorecard spot ~p already taken~n", [timestamp(), ScorecardLine]),
			-1;
		true ->
			get_score_for(ScorecardLine, Dice)
	end.

% return a set of three dice rolls
get_dice_roll() ->
	Set1 = get_dice_set(),
	Set2 = get_dice_set(),
	Set3 = get_dice_set(),
	[Set1, Set2, Set3].

get_dice_set() ->
	random:seed(now()),
	Die1 = round(random:uniform(5)) + 1,
	Die2 = round(random:uniform(5)) + 1,
	Die3 = round(random:uniform(5)) + 1,
	Die4 = round(random:uniform(5)) + 1,
	Die5 = round(random:uniform(5)) + 1,
	[Die1, Die2, Die3, Die4, Die5].






