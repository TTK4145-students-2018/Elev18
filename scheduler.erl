-module(scheduler).
-compile(export_all).

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
scheduler(WorldViews, Order) ->
	scheduler(WorldViews, Order, 100).

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
	Mem * (Num/2 + Place + Dist). %/2 is kinda arbitrary and very tunable

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
	Orders = element(1, WorldView),
	[First|_] = Orders,
	case order_manager:ideal_first(First, WorldView, element(1, Order), element(2, Order)) of
		true -> 1;
		false -> order_manager:find_position(Orders, Order, 2)
	end.



