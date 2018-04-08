-module(order_manager).
-compile(export_all).

start() ->
	%spawn(fun() -> order_manager([]) end).
	order_manager([]).

order_manager(Orders) ->
	io:format("order_manager: current orders: ~p~n", [Orders]),
	receive 
		{add, Floor} ->
			io:format("order_manager: adding floor: ~p~n", [Floor]),
			NewOrders = add_order(Orders, Floor),
			fsm ! {ev_new_order},
			worldview ! {orders, NewOrders},
			order_manager(NewOrders);
		{remove, Floor} ->
			io:format("order_manager: removing floor: ~p~n", [Floor]),
			NewOrders = remove_order(Orders, Floor),
			worldview ! {orders, NewOrders},
			order_manager(NewOrders);
		{clear} ->
			io:format("order_manager: clearing orders ~n"),
			worldview ! {orders, []},
			order_manager([]);
		{get_first, Pid} ->
			Pid ! get_first(Orders),
			order_manager(Orders);
		{get_all, Pid} ->
			Pid ! {orders, Orders},
			order_manager(Orders);
		{request_new_order} ->
			case get_first(Orders) of
				0 -> fsm ! {no_orders};
				_ -> fsm ! {ev_new_order}
			end
	end.

get_first([]) ->
	0;

get_first([H|T]) ->
	H.

add_order([], NewOrder) ->
	[NewOrder];

add_order(Orders, NewOrder) ->
	case lists:member(NewOrder, Orders) of
		true -> Orders;
		false -> Orders ++ [NewOrder]
	end.

remove_order(Orders, Order) ->
	case lists:member(Order, Orders) of
		true -> Orders -- [Order];
		false -> Orders
	end.
