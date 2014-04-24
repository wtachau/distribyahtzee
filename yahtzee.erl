%% @author Will Tachau, James Reinke
%%
%% Usage:
%% erl -compile yahtzee
%% erl -noshell -run yahtzee main tm1 -run init stop -noshell
%%

-module(yahtzee).

%% ====================================================================
%% API functions
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

	listen([]).

%% ====================================================================
%% Internal functions
%% ===========================`=========================================	
	
timestamp() ->
    {_, _, Micros} = now(), 
    {{_, _, _}, {Hour, Min, Sec}} = calendar:now_to_local_time(now()),
    lists:concat([Hour, ":", Min, ":", Sec, ".", Micros]).

% Continuous listening method
listen(Players)->
	receive
		{login, PID, {Username, Password}} ->
			io:format("~p Received login from ~p at ~p~n",[timestamp(), Username, PID]),

			% monitor it
			erlang:monitor(process, PID),

			% create login ticket for player
			LoginTicket = make_ref(),
			PID ! {logged_in, self(), LoginTicket},

			io:format("~p All Players: ~p~n", [timestamp(), Players++[{Username, PID, LoginTicket}]]),

			% Remember player
			listen(Players++[{Username, PID, LoginTicket}]);

		%{start_tournament, PID, NumberPlayers} ->
		%
		%	% set flag to YES, remember number somehow
		%	ok,

		{'DOWN', MonitorReference, process, PID, Reason} ->
			io:format("process ~p died!~n", [PID]),
			listen(Players);

		{__, PID, Data} ->
			io:format("~p Received unknown message from ~p~n", [timestamp(), PID]),
			listen(none)
	end.