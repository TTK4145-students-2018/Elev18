-module(scheduler).
-export([scheduler/2]).

% Evaluates cost for a list of worldviews to accept an order. Returns the ID
% of the elevator that results in the lowest cost. Deterministic, so will return
% the lowest ID in case of a tie.

% Cost from worldview based on:
% Number of orders currently taken
% Absolute distance from current position to the floor of the order
% Placement of the new order (relative to the other orders)
% Whether the new order would cause a new change in direction

% If the order already exists in a worldview, its cost will be returned
% as 0, and that elevator will "take" the order. (order_manager does the rest)
% If the worldview is in state init, the cost will be significantly increased,
% causing another elevator to take the order.

scheduler(WorldViews, Order) ->
	io:format("WorldViews: ~p~n", [WorldViews]),
	SortedViews = lists:keysort(1, WorldViews),
	scheduler(SortedViews, Order, 100).

scheduler([], _, _) ->
	-1;

scheduler(WorldViews, Order, Cost) ->
	% returns the ID of the node that is considered to
	% be the best fit for the order.
	[WorldView|RestViews] = WorldViews,
	CurrentCost = get_cost(WorldView, Order),
	case CurrentCost < Cost of
		true -> 
			RestID = scheduler(RestViews, Order, CurrentCost), 
			case RestID > -1 of
				true -> RestID;
				false -> element(1, WorldView)
			end;
		false -> scheduler(RestViews, Order, Cost)
	end.

get_cost(WorldView, Order) ->
	Num = number_of_orders(WorldView),
	Dist = get_distance(WorldView, Order),
	Place = order_placement(WorldView, Order),
	Mem = member(WorldView, Order),
	Init = init(WorldView),
	Dir = direction_change(WorldView, Order),
	Mem * (Num/2 + Place + Dist + 10*Init + 2*Dir).

member(WorldView, Order) ->
	%returns 0 if Order already exists in WorldView
	Orders = element(4, WorldView),
	case lists:member(Order, Orders) of
		true -> 0;
		false -> 1
	end.

number_of_orders(WorldView) ->
	Orders = element(4, WorldView),
	length(Orders).

get_distance(WorldView, Order) ->
	% returns distance between current distance and order
	% floor. Currently regardless of the direction the elevator
	% must travel before taking it.
	LastFloor = element(3, WorldView),
	OrderFloor = element(1, Order),
	abs(LastFloor - OrderFloor).

order_placement(WorldView, Order) ->
	Orders = element(4, WorldView),
	case Orders == [] of
		true ->
			1;
		false ->
			[First|_] = Orders,
			case order_manager:ideal_first(First, WorldView, element(1, Order), element(2, Order)) of
				true -> 1;
				false -> order_manager:find_position(Orders, Order, 2)
			end
	end.

init(WorldView) ->
	% returns 1 if the node is in the init state.
	% elevator should not take an order during this
	% state, as not all processes are running yet.
	State = element(2, WorldView),
	case State == init of
		true ->
			1;
		false ->
			0
	end.

direction_change(WorldView, Order) ->
	% penalizes adding a new direction for the first time.
	% i.e. if there are only hall_up and cab-orders, adding
	% hall_down will be penalized. If both directions are 
	% represented, it will no longer be penalized.
 	OrderDir = element(2, Order),
 	Orders = element(4, WorldView),
 	case OrderDir of
 		hall_down ->
 			Other = hall_up;
 		hall_up ->
 			Other = hall_down;
 		cab ->
 			Other = arbitrary
 	end,
 	case lists:keymember(Other, 2, Orders) of
 		true ->
 			case lists:keymember(OrderDir, 2, Orders) of
 				true ->
 					0;
 				false ->
 					1
 			end;
 		false ->
 			0
 	end.