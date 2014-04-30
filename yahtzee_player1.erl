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
-import(yahtzee_lib, [get_total_of/2, 
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
					make_decision/2]).

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
	{yahtzee_manager, TManager} ! Msg,

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
			% If we can already return something (i.e. a large straight, yahtqzee)
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



%% ====================================================================
%%   Other Helper Functions
%% ====================================================================	


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
if 
	RollNumber =:= 1 ->
		make_decision(Dice, Scorecard);
	RollNumber =:= 2 ->
		make_decision(Dice, Scorecard);
	true ->
		make_decision(Dice, Scorecard).

get_random_die() ->
	random:seed(now()),
	Rand = round(random:uniform()),
	if
		Rand == 1 ->
			true;
		true ->
			false
	end.





























