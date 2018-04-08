-module(basic_test).
-export([init_drive/0]).

-define(DELAY, 100).

% This should make the elevator move to the first floor,
% then move between the first and fourth floor

init_drive() ->
	FloorState = driver:get_floor_sensor_state(driver),
	case FloorState of
		0 ->
			io:format("Initialized!"),
			drive_up();
		_ ->
			driver:set_motor_direction(driver, down),
			timer:sleep(?DELAY),
			init_drive()
	end.



drive_up() ->
	driver:set_motor_direction(driver, up),
	timer:sleep(?DELAY),
	FloorState = driver:get_floor_sensor_state(driver),
	case FloorState of
		3 ->
			drive_down();
		_ ->
			drive_up()
	end.

drive_down() ->
	driver:set_motor_direction(driver, down),
	timer:sleep(?DELAY),
	FloorState = driver:get_floor_sensor_state(driver),
	case FloorState of
		0 ->
			drive_up();
		_ ->
			drive_down()
	end.

	