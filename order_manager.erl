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


% Unsure where to put this, but if worldview is stored here then maybe this is the place
set_ID(NewID) ->
	ID = NewID.