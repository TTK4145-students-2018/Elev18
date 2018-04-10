-module(order_manager).
-compile(export_all).

% TODO:
% make orders tuples instead of numbers on form:
% {Floor, call_type}, for example {3, hall_down}.
% Then find way of placing a new call in the correct
% spot.

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
				-1 -> fsm ! {no_orders};
				_ -> fsm ! {ev_new_order}
			end
	end,
	order_manager(Orders).

suitable_position([], _, Position, _) ->
	Position;

suitable_position([H|T], Order, Position, LastDirection) ->
	OrderFloor = element(1, Order),
	OrderDir = element(2, Order),
	case OrderDir == LastDirection and OrderDir == hall_down and OrderFloor of
		true -> 
		false ->
	end.

ideal(NextFloor, OrderFloor, hall_down, LastDirection) ->
	(LastDirection == hall_down) and (OrderFloor > NextFloor);

ideal(NextFloor, OrderFloor, hall_up, LastDirection) ->
	Normal = (LastDirection == hall_up) and (OrderFloor < NextFloor),
	Special = (LastDirection == hall_down and )


get_first([]) ->
	-1;

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

%find_position(Orders, Order) ->
	% returns suitable position for Order to be placed
	% within Orders.
	%RECURSION