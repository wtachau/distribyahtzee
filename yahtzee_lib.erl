-module(yahtzee_lib).
-export([get_total_of/2, get_total/1, get_score_for/2, in_small_straight/2, in_straight/1, get_min/2, get_max/2, get_most_common/1, most_common/2, get_frequency/1, add_freq/2]).

%% ====================================================================
%%   Detect if a Pattern is in a Hand
%% ====================================================================	

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
			30;
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