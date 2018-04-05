-module(scheduler).
-export([get_cost/2]).

% Calculates cost, and determines whether it is optimal 
% for this elevator to perform a task instead of one of the
% others. Keeps track of worldview* and own state (from fsm) to
% serve as a basis of calculations. Needs the same worldview as
% the other schedulers, to make sure every scheduler reaches
% decisions based on correct information about the entire system. 

% TODO:
% Calculate cost of moving a number of floors
% Calculate cost of changing direction
% 

get_cost(Orders, NewOrder) ->
	case lists:member(NewOrder, Orders) of
		true -> 0;
		false -> 1
	end.

%get_distance([Orders], Order) ->
%	end.
	% returns the number of floors the new order
	% is away from the state of the elevator (considering
	% current floor, and direction)
	% {Floor, Direction} = driver:get_state(),