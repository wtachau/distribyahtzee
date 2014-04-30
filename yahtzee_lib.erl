-module(yahtzee_lib).
-export([get_total_of/2, 
	get_total/1, 
	get_score_for/2, 
	in_small_straight/2, 
	in_straight/1, 
	get_min/2, 
	get_max/2, 
	get_most_common/1, 
	most_common/2, 
	get_frequency/1, 
	add_freq/2,
	get_straight_choice/1,
	select_dice_goal/2,
	choose/2,
	get_difference/1,
	sort/1,
	main/0,
	make_decision/2]).

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

%% ====================================================================
%%   Other Helper Functions
%% ====================================================================	
% Returns the die a player should keep if the player want to pursue a that hand
% return empty list when we have examined all of our die


select_dice(Argument, Dice) ->
	if
		Argument =:= 1 -> select_dice_goal(1, Dice);
		Argument =:= 2 -> select_dice_goal(2, Dice);
		Argument =:= 3 -> select_dice_goal(3, Dice);
		Argument =:= 4 -> select_dice_goal(4, Dice);
		Argument =:= 5 -> select_dice_goal(5, Dice);
		Argument =:= 6 -> select_dice_goal(6, Dice);
		Argument =:= 7 -> select_dice_goal(7, Dice);
		Argument =:= 8 -> select_dice_goal(8, Dice);
		Argument =:= 9 -> select_dice_goal(9, Dice);
		Argument =:= 10 -> select_dice_goal(10, Dice);
		Argument =:= 11 -> select_dice_goal(11, Dice);
		Argument =:= 12 -> select_dice_goal(12, Dice);
		Argument =:= 13 -> select_dice_goal(13, Dice);
		true -> select_dice_goal(1, Dice)
	end.

select_dice_goal(_, []) -> [];

% ones
select_dice_goal(1, Dice) ->
	Examine = hd(Dice),
	if 
		Examine =:= 1 -> [true] ++ select_dice_goal(1, tl(Dice));
		true -> [false] ++ select_dice_goal(1, tl(Dice))
	end;

% twos
select_dice_goal(2, Dice) -> 
	Examine = hd(Dice),
	if 
		Examine =:= 2 -> [true] ++ select_dice_goal(2, tl(Dice));
		true -> [false] ++ select_dice_goal(2, tl(Dice))
	end;
% threes
select_dice_goal(3, Dice) -> 
	Examine = hd(Dice),
	if 
		Examine =:= 3 -> [true] ++ select_dice_goal(3, tl(Dice));
		true -> [false] ++ select_dice_goal(3, tl(Dice))
	end;
% fours
select_dice_goal(4, Dice) ->
	Examine = hd(Dice),
	if 
		Examine =:= 4 -> [true] ++ select_dice_goal(4, tl(Dice));
		true -> [false] ++ select_dice_goal(4, tl(Dice))
	end;
% fives
select_dice_goal(5, Dice) -> 
	Examine = hd(Dice),
	if 
		Examine =:= 5 -> [true] ++ select_dice_goal(5, tl(Dice));
		true -> [false] ++ select_dice_goal(5, tl(Dice))
	end;
% sixes
select_dice_goal(6, Dice) -> 
	Examine = hd(Dice),
	if 
		Examine =:= 6 -> [true] ++ select_dice_goal(6, tl(Dice));
		true -> [false] ++ select_dice_goal(6, tl(Dice))
	end;
% three of a kind
select_dice_goal(7, Dice) -> 
	Val = get_most_common(Dice),
	% choose the dice with the highest frequency
	choose([Val, Val, Val, Val, Val], Dice);

% four of a kind
select_dice_goal(8, Dice) -> 
	Val = get_most_common(Dice),
	% choose the dice with the highest frequency
	choose([Val, Val, Val, Val, Val], Dice);

% full house
select_dice_goal(9, Dice) -> 
	Val1 = get_most_common(Dice),
	% choose the dice with the two highest frequencies
	Val2 = get_most_common(Dice -- [Val1, Val1, Val1, Val1, Val1]),
	choose([Val1, Val1, Val1, Val2, Val2], Dice);

% small straight
select_dice_goal(10, Dice) -> 
	choose(get_straight_choice(Dice), Dice);
% large straight
select_dice_goal(11, Dice) ->
	choose(get_straight_choice(Dice), Dice);
% yahtzee
select_dice_goal(12, Dice) -> 
	Val = get_most_common(Dice),
	% choose the dice with the highest frequency
	choose([Val, Val, Val, Val, Val], Dice);

% chance
select_dice_goal(13, Dice) ->
	% remove all 1's 2's and 3's
	choose(Dice--[1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3], Dice).

% once we have figured out the dice numbers we want, we create a boolean array as our choice
choose(_, []) -> [];

choose([], Dice) -> [false] ++ choose([], tl(Dice));

choose(Dice_Numbers, Dice) ->
	New = Dice_Numbers -- [hd(Dice)],
	if 
		length(New) =/= length(Dice_Numbers) -> 
			[true] ++ choose(Dice_Numbers--[hd(Dice)], tl(Dice));
		true ->
			[false] ++ choose(Dice_Numbers, tl(Dice))
	end.

% returns a subset of dice that consist of the smallest difference
% inefficient; coulde use dynamic programming here but we only have sets of 5



get_straight_choice([]) -> [];

get_straight_choice([H|[]]) -> [];

get_straight_choice([H|T]) ->
	% look in sorted sublists by dropping the first and last elements
	FirstCall = get_straight_choice(sort(T)),
	SecondCall = get_straight_choice([H|T] -- [lists:last(sort([H|T]))]),
	NoDupes = lists:usort([H|T]),
	Difference = get_difference(sort([H|T])),
	if 
		Difference =:= (length([H|T]) - 1) andalso length(NoDupes) =:= length([H|T]) -> [H|T];
		true -> 
				if 
					length(FirstCall) > length(SecondCall) -> FirstCall;
					true -> SecondCall
				end
	end.


% empty tail
get_difference([H|[]]) -> 0;

get_difference(Dice) ->
	abs(hd(Dice) - hd(tl(Dice))) + get_difference(tl(Dice)).


sort([]) -> [];
sort(List) -> sort(List, []).

sort([], Sorted) -> Sorted;

sort([H|T], Sorted) ->
    {Max, Rest} = select_max(T, H, []),
    sort(Rest, [Max|Sorted]).

select_max([], Max, Rest) -> {Max, Rest};
select_max([H|T], Max, Rest) when H < Max ->
    select_max(T, Max, [H|Rest]);
select_max([H|T], Max, Rest) ->
    select_max(T, H, [Max|Rest]).

count_true([]) -> 0;
count_true([H|T]) ->
	if 
		H =:= true -> 
			1 + count_true(T);
		true -> 
			0 + count_true(T)
	end.

make_decision(Dice, Scorecard) ->
	Choice_Scores = get_choice_scores(Dice, Scorecard),
	io:format("Choice Scores: ~p ~n",[Choice_Scores]),
	Choice = lists:min(Choice_Scores),
	Decision = index_of(Choice, Choice_Scores),
	% the dice we would like to choose
	io:format("Decision: ~p ~n", [Decision]),
	select_dice(Decision, Dice).


get_choice_scores(Dice, Scorecard) -> get_choice_scores(Dice, Scorecard, 1).

get_choice_scores(Dice, [], Index) -> [];

get_choice_scores(Dice, Scorecard, Index) ->
	if 
		Index =:= 13 -> [count_true(select_dice(Index, Dice)) * hd(Scorecard)] ++ get_choice_scores(Dice, tl(Scorecard), Index+1);
		true-> [count_true(select_dice(Index, Dice)) * hd(Scorecard) * Index] ++ get_choice_scores(Dice, tl(Scorecard), Index+1)
	end.


% finds the index of an item
index_of(Item, List) -> index_of(Item, List, 1).
index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).





main() ->
	Scorecard = [-1,-1,-1,-1,25,-1,-1,-1,-1,-1,-1,-1,-1,-1],
	Dice = [4,4,4,5,6],
	One = make_decision(Dice, Scorecard),
	%One = choose([1,1,1,1,1], [1,1,1,5,6]),
	%Two = select_dice_goal(8, [4,4,4,2,2]),
	%Three = select_dice_goal(9, [4,4,4,2,2]),
	io:format("~p ~n", [One]).







