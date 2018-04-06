-module(fsm).
-export([start/1]).

% the finite state machine of our system
% is to have control over the internal
% state of the node it runs on,
% based on events received from the driver

% TODO:
% event handler for for example set indicators and such
% interface with a poller that sends events
% FIND OUT IF EVENT_MANAGER SHOULD SEE IF DEST REACHED
% OR IF FSM SHOULD ASK ORDER_MANAGER?
% add worldview tuple {State, LastFloor, Dir, ID, LocalOrders}
% to all states


start(Driver) ->
	spawn(fun() -> st_init(Driver) end),
	spawn(fun() -> driver:start()). %is this right to make gen_server work?

st_init(Driver) ->
	io:format("fsm: initializing ~n"),
	driver:set_motor_direction(Driver, down),
	receive ev_ground_floor_reached ->
		io:format("fsm: elevator initialized, behold my initial glory ~n"),
		driver:set_motor_direction(Driver, stop),
		st_idle(Driver);
		Other ->
			io:format("fsm_init: received garbage: ~p~n", [Other]),
			st_init(Driver)

	after 10000 ->
		io:format("fsm: init timed out, trying again ~n"),
		st_init(Driver)
	end.


st_idle(Driver) ->
	io:format("fsm: elevator idle ~n"),
	receive 
		ev_order_received ->
			st_moving(Driver);

		ev_emergency_stop ->
			st_emergency(Driver);

		Other ->
			io:format("fsm_idle: received garbage: ~p~n", [Other]),
			st_idle(Driver)
	end.


st_moving(Driver) ->
	io:format("fsm: moving ~n"),
	driver:set_motor_direction(Driver, get_direction(Driver))
	receive 
		{ev_floor_passed, floor} ->
			driver:set_floor_indicator(Driver, floor);

		{ev_destination_reached, floor} ->
			io:format("fsm: destination reached ~n"),
			driver:set_floor_indicator(Driver, floor),
			driver:set_motor_direction(Driver, stop),
			st_doors_open(Driver);

		ev_emergency_stop ->
			st_emergency(Driver);

		Other ->
			io:format("fsm_moving: received garbage: ~p~n", [Other]),
			st_moving(Driver)

	end.

st_doors_open(Driver) ->
	io:format("doors opened ~n"),
	driver:set_door_open_light(Driver, on),

	after 2000 ->
		driver:set_door_open_light(Driver, off)
		receive
			ev_order_received ->
				st_moving(Driver);
	
			Other ->
				io:format("fsm_doors_open: received garbage: ~p~n", [Other])

		st_idle(Driver)
		end.

st_emergency(Driver) ->
	io:format("fsm: emergency state activated ~n"),
	driver:set_motor_direction(Driver, stop),
	driver:set_stop_button_light(Driver, on),
	receive
		ev_order_received ->
			st_moving(Driver);

		ev_emergency_stop ->
			st_emergency(Driver);

		Other ->
			io:format("fsm_emergency: received garbage: ~p~n", [Other]),
			st_emergency(Driver)

	end.


get_direction(Driver) -> 
    Driver ! {direction, request, self()},
    receive
		{direction, response, Direction} ->
	    	Direction
    end.
