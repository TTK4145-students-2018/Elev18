-module(fsm).
-export([start/0]).

% Finite state machine, that keeps control over the state of the elevator.
% Does some interfacing with driver, such as checking if the correct destination
% is reached, and stopping motor direction thereafter.

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
		true -> fsm ! {ev_floor_reached, DestFloor};
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
