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
	receive 
		{add, Order} ->
			io:format("order_manager: adding order: ~p~n", [Order]),
			worldview ! {request, wv, order_manager},
			receive {response, wv, WorldView} -> ok end,
			NewOrders = add_order(Orders, Order, WorldView),
			fsm ! {ev_new_order},
			worldview ! {orders, NewOrders},
			io:format("order_manager: new orders: ~p~n", [NewOrders]),

			Floor = element(1, Order),
			ButtonType = element(2, Order),
			driver:set_order_button_light(driver, ButtonType, Floor, on),
			order_manager(NewOrders);
		{remove, Floor} ->
			io:format("order_manager: removing order: ~p~n", [Floor]),
			NewOrders = remove_order(Orders, Floor),
			worldview ! {orders, NewOrders},
			io:format("order_manager: new orders: ~p~n", [NewOrders]),
			
			Floor = element(1, Order),
			ButtonType = element(2, Order),
			driver:set_order_button_light(driver, ButtonType, Floor, off),
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

add_order([], NewOrder, _) ->
	[NewOrder];

add_order(Orders, NewOrder, WorldView) ->
	[First|_] = Orders,
	OrderFloor = element(1, NewOrder),
	OrderDir = element(2, NewOrder),
	case lists:member(NewOrder, Orders) of
		true -> 
			Orders;
		false -> 
			case ideal_first(First, WorldView, OrderFloor, OrderDir) of
				true ->
					NewOrders = hf:list_insert(Orders, NewOrder, 1);
				false ->
					NewOrders = hf:list_insert(Orders, NewOrder, find_position(Orders, NewOrder, 2))
			end				
	end.

remove_order([], _) ->
	[];

remove_order(Orders, Floor) ->
	[First|_] = Orders,
	case (element(1, First) == Floor) of
		true ->
			NewOrders = lists:keydelete(Floor, 1, Orders),
			remove_order(NewOrders, Floor);
		false ->
			Orders
	end.

find_position([_|[]], _, Position) ->
	Position;

find_position([PrevOrder|NextOrders], Order, Position) ->
	% returns position for Order to be inserted in current orders.
	% NB! START WITH Position = 2, as ideal_first is used for the edge
	% case of checking if the order can be placed at the very beginning.
	% Calling it with Position = 1 would return one value too low.
	OrderFloor = element(1, Order),
	OrderDir = element(2, Order),
	[NextOrder|_] = NextOrders,
	case ideal(PrevOrder, NextOrder, OrderFloor, OrderDir) of
		true -> Position;
		false -> find_position(NextOrders, Order, Position + 1)
	end.	

ideal_first(NextOrder, WorldView, OrderFloor, OrderDir) ->
	% returns true if the order can be placed at the front of list, i.e. it
	% can do the order before the initial first order
	Position = element(3, WorldView),
	NextFloor = element(1, NextOrder),
	Between = ((OrderFloor > Position) and (OrderFloor < NextFloor)) or 
	((OrderFloor < Position) and (OrderFloor > NextFloor)),
	Cab = ((NextFloor == OrderFloor) and (OrderDir == cab)),
	case Position < NextFloor of
		true -> Dir = hall_up;
		false -> Dir = hall_down
	end,
	(Between and ((OrderDir == Dir) or (OrderDir == cab))) or Cab.

ideal(PrevOrder, NextOrder, OrderFloor, hall_down) ->
	% returns true if position between prevorder and nextorder is gucci
	LastFloor = element(1, PrevOrder),
	LastDir = element(2, PrevOrder),
	NextFloor = element(1, NextOrder),
	NextDir = element(2, NextOrder),
	Normal = (LastDir == hall_down) and (OrderFloor >= NextFloor) and (OrderFloor =< LastFloor),
	Special = (LastDir == hall_up) and (NextDir == hall_down) and (OrderFloor >= NextFloor),
	Cab = (LastFloor > NextFloor) and (OrderFloor =< LastFloor) and (OrderFloor >= NextFloor),
	Cab or Normal or Special;

ideal(PrevOrder, NextOrder, OrderFloor, hall_up) ->
	LastFloor = element(1, PrevOrder),
	LastDir = element(2, PrevOrder),
	NextFloor = element(1, NextOrder),
	NextDir = element(2, NextOrder),
	Normal = (LastDir == hall_up) and (OrderFloor =< NextFloor) and (OrderFloor >= LastFloor),
	Special = (LastDir == hall_down) and (NextDir == hall_up) and (OrderFloor =< NextFloor),
	Cab = (LastFloor < NextFloor) and (OrderFloor >= LastFloor) and (OrderFloor =< NextFloor),
	Cab or Normal or Special;

ideal(PrevOrder, NextOrder, OrderFloor, cab) ->
	LastFloor = element(1, PrevOrder),
	NextFloor = element(1, NextOrder),
	Equal = (OrderFloor == NextFloor),
	Up = (LastFloor < NextOrder) and (OrderFloor >= LastFloor) and (OrderFloor =< NextFloor),
	Down = (LastFloor > NextOrder) and (OrderFloor =< LastFloor) and (OrderFloor >= NextFloor),
	Up or Down or Equal.

get_first([]) ->
	-1;

get_first([H|_]) ->
	H.