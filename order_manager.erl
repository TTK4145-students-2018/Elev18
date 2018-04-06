-module(order_manager).
-export([start/0, add_order/2, remove_order/2]).

start() ->
	spawn(fun() -> order_manager([]) end).

order_manager(Orders) ->
	io:format("order_manager: current orders: ~p~n", [Orders]),
	receive 
		{add, Floor} ->
			io:format("order_manager: adding floor: ~p~n", [Floor]),
			NewOrders = add_order(Orders, Floor),
			order_manager(NewOrders);
		{remove, Floor} ->
			io:format("order_manager: removing floor: ~p~n", [Floor]),
			NewOrders = remove_order(Orders, Floor),
			order_manager(NewOrders);
		{get_first, Pid} ->
			Pid ! get_first(Orders),
			order_manager(Orders);
		{get_all, Pid} ->
			Pid ! Orders,
			order_manager(Orders)
	end.

get_first([H|T]) ->
	H.

add_order([], NewOrder) ->
	[NewOrder];

add_order(Orders, NewOrder) ->
	case lists:member(NewOrder, Orders) of
		true -> Orders;
		false -> Orders ++ [NewOrder]
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