Will Tachau, James Reinke

NOTES:

This was a very extensive assignment! A lot of work involved in logging in/ logging out, tournament manager/ match manager/ player interaction, bracket formation and having the tournament manager keep track of information and update it appropriately. 

The expected robustness of the code, and the degree to which fault tolerance was necessary, also made the design and implementation of the project significantly more challenging (but fun!). 

We also designed a yahtzee player… 


Included 


TESTING:

Some terminal output has been omitted for clarity. Output for the tests described below are found in files of the same name. In the output, notes are in ———— THIS FORMAT ———.

#################################################################################


Test 1 -  Standard test 
	
	Four players started, and then a request_tournament message for four players (u, u2, u3, u4) with 3 games each
	Bracket scheduled, u vs u2 and u3 vs u4. 
	As games are won (winners: u2 and u4), tournament_manager’s bracket is updated
	New match started, u2 vs u4
	u2 wins, updated in bracket structure:
		 {"u2","p",1,2,0,[898304131848],<5389.2.0>,#Ref<0.0.0.62>} means username u2, password “p”, 1 tournament won, 2 matches won, none lost, tournaments played in = TID 898304131848, PID and Ref.	
	Tournament_Manager also keeps track of it:
		[{898304131848,
   {"u2","p",0,0,0,[],<5389.2.0>,#Ref<0.0.0.62>},
   3,
   [{1,
     {"u2","p",0,0,0,[],<5389.2.0>,#Ref<0.0.0.62>},
     {"u4","p",0,0,0,[],<5391.2.0>,#Ref<0.0.0.102>}},
    {2,
     {"u","p",0,0,0,[],<5388.2.0>,#Ref<0.0.0.44>},
     {"u2","p",0,0,0,[],<5389.2.0>,#Ref<0.0.0.62>}},
    {3,
     {"u3","p",0,0,0,[],<5390.2.0>,#Ref<0.0.0.81>},
     {"u4","p",0,0,0,[],<5391.2.0>,#Ref<0.0.0.102>}}]}] is {Winner, GamesPerMatch, Bracket}.


#################################################################################


Test 2 - Not enough confirmed players

	u3 has logged out, and is unresponsive
	For simplicity, only 1 game per match
	Tournament Manager schedules a match between u4 and bye, which is immediately won by u4


#################################################################################


Test 3 - Not nearly enough confirmed players

	All players logged out except u
	Tournament requested with 4 players
	4-player bracket scheduled with 3 byes, player U beats two byes and wins


#################################################################################


Test 4 - Players tie

	Player module has been augmented so that players discard dice everytime, making multiple players’ strategies identical.
	After k/2 ties, the match is restarted in Standard Mode, and two sets of dice rolls are made each turn


#################################################################################


Test 5 - Players are logged out via monitors, but information kept

	Player u’s process is killed, Tournament Manager catches it via monitor and logs it out
	Information about username, password, tournaments won, matches won, etc is all preserved
	Player u2’s process killed same way
	Player u logs back in, associated with correct Player tuple
	Player u2 tries to log in with incorrect password, denied
	Player u2 logs back in with correct password and is successful


#################################################################################


Test 6 - One player crashes during matchplay

	Player u and u2 log in and begin match
	u2 crashes during game 1, forfeits game 1
	Match manager starts games 2 and 3 in case u2 logs back in
	u wins the match


#################################################################################


Test 7 - Both players crash during matchplay

	Player u and u2 log in and begin match
	u2 crashes during game 2, forfeits game 1
	u also crashes, unresponsive
	Match manager starts game 3 in case u2 or u logs back in
	Neither are responsive, so match is forfeited and ‘bye’ advances
	





