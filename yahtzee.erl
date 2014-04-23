%% @author Will Tachau, James Reinke
%%
%% Usage:
%% erl -compile yahtzee
%% erl -noshell -run yahtzee main networkname -run init stop -noshell
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
	global:register_name(list_to_atom(Name), self()),
	timer:sleep(1000),
	io:format("~p Registered as node ~p, name ~p, nodes ~p~n", [timestamp(), node(), Name, nodes()]),
	io:format("Registered names:~p~n",[global:registered_names()]),

	listen().

%% ====================================================================
%% Internal functions
%% ===========================`=========================================	
	
timestamp() ->
    {_, _, Micros} = now(), 
    {{_, _, _}, {Hour, Min, Sec}} = calendar:now_to_local_time(now()),
    lists:concat([Hour, ":", Min, ":", Sec, ".", Micros]).

% Continuous listening method
listen()->
	receive
		{login, PID, Data} ->
			io:format("~p Received login from ~p~n",[timestamp(), PID]),
			listen();
		{__, Pid, Data} ->
			io:format("~p Received unknown message~n", [timestamp()]),
			listen()
	end.