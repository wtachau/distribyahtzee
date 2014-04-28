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
-compile(export_all).
%-export([main/1]).

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
	register(yahtzee_manager, self()),
	% Connect to registry of first tournament manager
	io:format("~p Registered as node ~p, with ~p~n", [timestamp(), node(), nodes()]),

	% login to tournament managers
	login(TManager, Username, Password, []).



%% ====================================================================
%%   Helper functions
%% ====================================================================		
	
timestamp() ->
    {_, _, Micros} = now(), 
    {{_, _, _}, {Hour, Min, Sec}} = calendar:now_to_local_time(now()),
    lists:concat([Hour, ":", Min, ":", Sec, ".", Micros]).



%% ====================================================================
%%   Login/Logout functions
%% ====================================================================	

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
	


%% ====================================================================
%%   Listen function
%% ====================================================================	

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

			% Call aux function
			{DiceToKeep, ScorecardLine} = make_a_move(Scorecard, DiceList, RollNumber),
			
			% and send decision
			PID ! {play_action, self(), {Ref, TID, GID, RollNumber, DiceToKeep, ScorecardLine}},

			listen(Connections);

		{__, __, __} ->
			io:format("~p Received unknown message~n", [timestamp()]),
			listen(none)
	end.


%% ====================================================================
%%   Functions to Play the Games
%% ====================================================================	
make_a_move(Scorecard, Dice, RollNumber) ->

	% First, if we have to return a decision
	if
		RollNumber == 3 ->
			% return a decision!
			{Decision, Score} = make_decision(1, Scorecard, Dice),
			{Dice, Decision};
		true ->
			% If we can already return something (i.e. a large straight, yahtzee)
			ShouldReturn = should_return_now(Scorecard, Dice),
			if
				ShouldReturn >= 1 ->
					{Dice, ShouldReturn};
				true ->
					% decide which dice to keep
					KeepDice = keep_dice(Dice, Scorecard, RollNumber),
					{KeepDice, 0}
			end

	end.


% Make a decision. Must return valid scorecard entry
make_decision(14, _, _) ->
	{-1,-1};
make_decision(CurrentIndex, Scorecard, Dice) ->
	% Return highest valid scorecard entry
	io:format("looking at ~p, ~p~n", [CurrentIndex, Scorecard]),
	CurScore = get_score_for(CurrentIndex, Dice),
	CurNotUsed = lists:nth(CurrentIndex, Scorecard),

	{RestIndex, RestScore} = make_decision(CurrentIndex+1, Scorecard, Dice),
	if
		CurScore > RestScore andalso CurNotUsed < 0 ->
			{CurrentIndex, CurScore};
		true ->
			{RestIndex, RestScore}
	end.


% Given a scorecard entry and the current set of dice, return what the score would be
get_score_for(1, Dice) -> get_total_of(Dice, 1);
get_score_for(2, Dice) -> get_total_of(Dice, 2);
get_score_for(3, Dice) -> get_total_of(Dice, 3);
get_score_for(4, Dice) -> get_total_of(Dice, 4);
get_score_for(5, Dice) -> get_total_of(Dice, 5);
get_score_for(6, Dice) -> get_total_of(Dice, 6);
get_score_for(7, Dice) -> 		% 3 of a kind
	MostCommon = get_most_common(Dice),
	TotalMostCommon = get_total_of(Dice, MostCommon),
	if
		TotalMostCommon >= MostCommon * 3 ->
			get_total(Dice);
		true ->
			0
	end;
get_score_for(8, Dice) -> 		% 4 of a kind
	MostCommon = get_most_common(Dice),
	TotalMostCommon = get_total_of(Dice, MostCommon),
	if
		TotalMostCommon >= MostCommon * 4 ->
			get_total(Dice);
		true ->
			0
	end;
get_score_for(9, Dice) -> 		% Full house
	Frequencies = get_frequency(Dice),
	if
		length(Frequencies) == 2 ->
			[{Val1, Len1}, {Val2, Len2}] = Frequencies,
			if
				Len1 == 2 ->
					25;
				Len1 == 3 ->
					25;
				true ->
					0
			end;
		true ->
			0
	end;
get_score_for(10, Dice) -> 		% small straight
	SmallStraight = in_small_straight(Dice, 1),
	if
		SmallStraight == true ->
			25;
		true ->
			0
	end;
get_score_for(11, Dice) -> 		% large straight
	LargeStraight = in_straight(Dice),
	if
		LargeStraight == true ->
			40;
		true ->
			0
	end;
get_score_for(12, Dice) -> 		% yahtzee
	AllOne = length(get_frequency(Dice)),
	if
		AllOne == 1 ->
			50;
		true ->
			0
	end;
get_score_for(13, Dice) -> get_total(Dice);
get_score_for(_, Dice) ->
	-1. %should never reach this




%% ====================================================================
%%   Helper Functions to Detect Straight
%% ====================================================================	

in_small_straight(Dice, 5) ->
	in_straight(lists:sublist(Dice, 4));
in_small_straight(Dice, Num) ->
	SubSet = lists:sublist(Dice, Num-1) ++ lists:nthtail(Num, Dice),
	StraightSubSet = in_straight(SubSet),
	if
		StraightSubSet == true ->
			true;
		true ->
			in_small_straight(Dice, Num+1)
	end.


in_straight(Dice) ->
	FreqList = get_frequency(Dice),
	Length = length(Dice),
	LengthFreq = length(FreqList),
	Min = get_min(FreqList, 7),
	Max = get_max(FreqList, 0),
	if
		Max - Min + 1 == Length ->
			if
				LengthFreq == Length ->
					true;
				true ->
					false
			end;
		true ->
			false
	end.

% get min of frequency list
get_min([], MinSoFar) ->
	MinSoFar;
get_min([{Val1, _}], MinSoFar) ->
	if
		Val1 < MinSoFar ->
			Val1;
		true ->
			MinSoFar
	end;
get_min([{Val1, _}| Rest], MinSoFar) ->
	if
		Val1 < MinSoFar ->
			get_min(Rest, Val1);
		true ->
			get_min(Rest, MinSoFar)
	end.


% get max of frequency list
get_max([], MaxSoFar) ->
	MaxSoFar;
get_max([{Val1, _}], MaxSoFar) ->
	if
		Val1 > MaxSoFar ->
			Val1;
		true ->
			MaxSoFar
	end;
get_max([{Val1, _}| Rest], MaxSoFar) ->
	if
		Val1 > MaxSoFar ->
			get_max(Rest, Val1);
		true ->
			get_max(Rest, MaxSoFar)
	end.


%% ====================================================================
%%   Helper Functions to Find Most Common
%% ====================================================================	

% From a frequency list, return most common
get_most_common(Dice) ->
	FreqList = get_frequency(Dice),
	{MostCommon, _} = most_common(FreqList, {0,0}),
	MostCommon.

% Get the most common element
most_common([], X) ->
	X;
most_common([{Val, Freq}], {CurMax, MaxFreq}) ->
	if
		Freq > MaxFreq ->
			{Val, Freq};
		true ->
			{CurMax, MaxFreq}
	end;
most_common([{Val, Freq} | Rest], {CurMax, MaxFreq}) ->
	if
		Freq > MaxFreq ->
			most_common(Rest, {Val, Freq});
		true ->
			most_common(Rest, {CurMax, MaxFreq})
	end.

% Get the frequency of each element in a list
get_frequency([]) ->
	[];
get_frequency([X|Xs]) ->
	add_freq({X, 1}, get_frequency(Xs)).

% helper function for getting the frequency
add_freq({Val, Num}, []) ->
	[{Val, Num}];
add_freq({Val, Num}, [{Val1, Num1}]) ->
	if
		Val == Val1 ->
			[{Val, Num+Num1}];
		true ->
			[{Val, Num}, {Val1, Num1}]
	end;
add_freq({Val, Num}, [{Val1, Num1}|Xs]) ->
	if
		Val == Val1 ->
			[{Val, Num+Num1}] ++ Xs;
		true ->
			[{Val1, Num1}]++add_freq({Val,Num}, Xs)
	end.


%% ====================================================================
%%   Other Helper Functions
%% ====================================================================	


% Return total of a given number
get_total_of([], _) ->
	0;
get_total_of([X|Xs], Target) ->
	if
		X == Target ->
			X + get_total_of(Xs, Target);
		true ->
			get_total_of(Xs, Target)
	end.

% Return total sum of dice
get_total([]) ->
	0;
get_total([X|Xs]) ->
	X + get_total(Xs).



% If we should return a decision (full house, small/large straight, yahtzee) rather than keep rolling
should_return_now(Scorecard, Dice) ->

	% if it's a yahztee, and one hasn't been scored yet
	IsYahtzee = get_score_for(12, Dice),
	YahtzeeAlready = lists:nth(12, Scorecard),

	% if it's a large straight, and one hasn't been scored yet
	IsLargeStraight = get_score_for(11, Dice),
	LargeStraightAlready = lists:nth(11, Scorecard),

	% if it's a small straight, and one hasn't been scored yet
	IsSmallStraight = get_score_for(10, Dice),
	SmallStraightAlready = lists:nth(10, Scorecard),

	% if it's a full house, and one hasn't been scored yet
	IsFullHouse = get_score_for(9, Dice),
	FullHouseAlready = lists:nth(9, Scorecard),

	if
		IsYahtzee > 0 andalso YahtzeeAlready < 0 ->
			12;
		IsLargeStraight > 0 andalso LargeStraightAlready < 0 ->
			11;
		IsSmallStraight > 0 andalso SmallStraightAlready < 0 ->
			10;
		IsFullHouse > 0 andalso FullHouseAlready < 0 ->
			9;
		true ->
			0
	end.



% Decide which dice to keep (return list of booleans)
keep_dice(Dice, Scorecard, RollNumber) ->
	% FIXME - strategy!
	Keep1 = round(random:uniform()),
	Keep2 = round(random:uniform()),
	Keep3 = round(random:uniform()),
	Keep4 = round(random:uniform()),
	Keep5 = round(random:uniform()),
	[Keep1, Keep2, Keep3, Keep4, Keep5].


























