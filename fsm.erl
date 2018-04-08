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
		{floor_reached, 0} ->
			io:format("fsm: elevator initialized, behold my initial glory ~n"),
			driver:set_motor_direction(driver, stop),
			%st_idle();
			st_idle();
		Other ->
			io:format("fsm_init: received garbage: ~p~n", [Other]),
			st_init(ID)

	after 10000 ->
		io:format("fsm: init timed out, trying again ~n"),
		st_init(ID)
	end.


st_idle() ->
	io:format("fsm: elevator idle ~n"),
	worldview ! {state, idle},
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
	worldview ! {state, moving},
	driver:set_motor_direction(driver, get_direction()),
	worldview ! {direction, get_direction()},
	worldview ! {request, fsm},
	receive {WorldView} -> ok end,
	[Dest|Rest] = element(4, WorldView),
	receive 
		{ev_floor_reached, Floor} ->
			worldview ! {floor, Floor},
			case Dest =:= Floor of
				true ->
					io:format("fsm: destination reached ~n"),
					driver:set_motor_direction(driver, stop),
					worldview ! {direction, stop},
					order_manager ! {remove, Floor},
					st_doors_open()
			end;

		{ev_emergency_stop} ->
			st_emergency();

		Other ->
			io:format("fsm_moving: received garbage: ~p~n", [Other]),
			st_moving()
	end.

st_doors_open() ->
	io:format("doors opened ~n"),
	worldview ! {state, doors_open},
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
	worldview ! {state, emergency},
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


get_direction() -> 
    order_manager ! {direction, request, self()},
    receive
		{direction, response, Direction} ->
	    	Direction
    end.

% Unsure where to put this, but if worldview is stored here then maybe this is the place
%set_ID(NewID) ->
%	ID = NewID.

