-module(order_manager).



add_order([Orders], [NewOrder]) ->
	[Orders] ++ [NewOrder].

add_order(_, [NewOrder]) ->
	[NewOrder].

get_cost([Orders], Order) ->
	
get_distance([Orders], Order) ->
	% returns the number of floors the new order
	% is away from the state of the elevator (considering
	% current floor, and direction)
	{Floor, Direction} = driver:get_state().