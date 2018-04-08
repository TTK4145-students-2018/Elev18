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

%WorldView ~ {ID, state, LastFloor, LocalOrders, dir}
% access elements with: element(x, WorldView)
% ex. element(1, WorldView) returns ID

start(ID) ->
	spawn(fun() -> st_init(ID) end).

st_init(ID) ->
	io:format("fsm: initializing ~n"),
	driver:set_motor_direction(driver, down),
	receive 
		{ev_ground_floor_reached} ->
			io:format("fsm: elevator initialized, behold my initial glory ~n"),
			driver:set_motor_direction(driver, stop),
			%WorldView = {ID, idle, 1, [], stop},
			st_idle();
			%st_idle(WorldView);
		Other ->
			io:format("fsm_init: received garbage: ~p~n", [Other]),
			st_init(ID)

	after 10000 ->
		io:format("fsm: init timed out, trying again ~n"),
		st_init(ID)
	end.


st_idle() ->
	io:format("fsm: elevator idle ~n"),
	order_manager ! {request_new_order},
	receive 
		{ev_new_order} ->
			st_moving();

		{ev_emergency_stop} ->
			st_emergency();

		Other ->
			io:format("fsm_idle: received garbage: ~p~n", [Other]),
			st_idle()
	end.


st_moving() ->
	io:format("fsm: moving ~n"),
	driver:set_motor_direction(driver, get_direction(driver)),
	receive 
		{ev_floor_passed, Floor} ->
			driver:set_floor_indicator(driver, Floor);

		{ev_destination_reached, Floor} ->
			io:format("fsm: destination reached ~n"),
			driver:set_floor_indicator(driver, Floor),
			driver:set_motor_direction(driver, stop),
			order_manager ! {remove, Floor},
			st_doors_open();

		{ev_emergency_stop} ->
			st_emergency();

		Other ->
			io:format("fsm_moving: received garbage: ~p~n", [Other]),
			st_moving()

	end.

st_doors_open() ->
	io:format("doors opened ~n"),
	driver:set_door_open_light(driver, on),
	receive
	after 2000 ->
		driver:set_door_open_light(driver, off),
		order_manager ! {request_new_order},
		receive
			{ev_new_order} ->
				st_moving();
			{no_orders} ->
				st_idle()
		end
	end.

st_emergency() ->
	io:format("fsm: emergency state activated ~n"),
	driver:set_motor_direction(driver, stop),
	driver:set_stop_button_light(driver, on),
	order_manager ! {clear},
	receive
		{ev_new_order} ->
			st_moving();

		{ev_emergency_stop} ->
			st_emergency();

		Other ->
			io:format("fsm_emergency: received garbage: ~p~n", [Other]),
			st_emergency()
	end.


get_direction(driver) -> 
    driver ! {direction, request, self()},
    receive
		{direction, response, Direction} ->
	    	Direction
    end.

% Unsure where to put this, but if worldview is stored here then maybe this is the place
%set_ID(NewID) ->
%	ID = NewID.

