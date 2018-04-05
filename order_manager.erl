-module(order_manager).
-export([add_order/2, remove_order/2]).


add_order([], NewOrder) ->
	[NewOrder];

add_order(Orders, NewOrder) ->
	case lists:member(NewOrder, Orders) of
		true -> Orders;
		false -> Orders ++ NewOrder
	end.

remove_order([], Order) ->
	io:format("Orders are already empty, can't remove ~p~n", [Order]),
	[];

remove_order(Orders, Order) ->
	case lists:member(Order, Orders) of
		true -> Orders -- [Order];
		false -> Orders
	end.

get_cost(Orders, NewOrder) ->
	
%get_distance([Orders], Order) ->
%	end.
	% returns the number of floors the new order
	% is away from the state of the elevator (considering
	% current floor, and direction)
	% {Floor, Direction} = driver:get_state(),