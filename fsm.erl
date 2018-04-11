-module(fsm).
-export([start/0]).

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

start() ->
	st_init().

st_init() ->
	io:format("fsm: initializing ~n"),
	driver:set_motor_direction(driver, down),
	receive 
		{ev_floor_reached, 0} ->
			io:format("fsm: elevator initialized, behold my initial glory ~n"),
			driver:set_motor_direction(driver, stop),
			%st_idle();
			st_idle();
		Other ->
			io:format("fsm_init: received garbage: ~p~n", [Other]),
			st_init()

	after 10000 ->
		io:format("fsm: init timed out, trying again ~n"),
		st_init()
	end.


st_idle() ->
	% io:format("fsm: elevator idle ~n"),
	worldview ! {state, idle},
	%order_manager ! {request_new_order},
	receive 
		{ev_new_order} ->
			st_moving();

		{ev_emergency_stop} ->
			st_emergency();

		{no_orders} ->
			st_idle();

		Other ->
			io:format("fsm_idle: received garbage: ~p~n", [Other]),
			st_idle()
	end.

st_moving() ->
	io:format("fsm: moving ~n"),
	worldview ! {state, moving},
	worldview ! {request, direction, fsm},
	receive {response, direction, Direction} -> ok end,
	driver:set_motor_direction(driver, Direction),
	worldview ! {direction, Direction},
	worldview ! {request, wv, fsm},
	receive {response, wv, WorldView} -> ok end,
	[Dest|_] = element(4, WorldView),
	DestFloor = element(1, Dest),
	case Direction == stop of
		true -> fsm ! {ev_floor_reached, element(1, Dest)};
		false -> ok
	end,
	receive 
		{ev_floor_reached, Floor} ->
			% worldview ! {floor, Floor}, sending from EM instead
			case DestFloor == Floor of
				true ->
					io:format("fsm: destination reached ~n"),
					driver:set_motor_direction(driver, stop),
					worldview ! {direction, stop},
					order_manager ! {remove, Floor},
					st_doors_open();
				false ->
					st_moving()
			end;

		{ev_emergency_stop} ->
			st_emergency();

		Other ->
			io:format("fsm_moving: received garbage: ~p~n", [Other]),
			st_moving()
	end.

st_doors_open() ->
	io:format("fsm: doors opened ~n"),
	worldview ! {state, doors_open},
	driver:set_door_open_light(driver, on),
	receive
	after 2000 ->
		driver:set_door_open_light(driver, off),
		%st_moving()
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
