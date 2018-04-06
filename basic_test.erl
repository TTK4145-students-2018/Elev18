-module(basic_test).
-export([start/0]).

-define(DELAY, 100).

% This should make the elevator move to the first floor,
% then move between the first and fourth floor

start() ->
	{ok, DriverPid} = driver:start(),
	init_drive(DriverPid).

init_drive(DriverPid) ->
	FloorState = driver:get_floor_sensor_state(DriverPid),
	case FloorState of
		0 ->
			io:format("Initialized!"),
			drive_up(DriverPid);
		_ ->
			init_drive(DriverPid, down)
	end.

init_drive(DriverPid, Direction) ->
	driver:set_motor_direction(DriverPid, Direction),
	timer:sleep(?DELAY),
	FloorState = driver:get_floor_sensor_state(DriverPid),
	case FloorState of
			0 ->
				io:format("Initialized!"),
				drive_up(DriverPid);
			_ ->
				init_drive(DriverPid)
	end.


drive_up(DriverPid) ->
	driver:set_motor_direction(DriverPid, up),
	timer:sleep(?DELAY),
	FloorState = driver:get_floor_sensor_state(DriverPid),
	case FloorState of
		3 ->
			drive_down(DriverPid);
		_ ->
			drive_up(DriverPid)
	end.

drive_down(DriverPid) ->
	driver:set_motor_direction(DriverPid, down),
	timer:sleep(?DELAY),
	FloorState = driver:get_floor_sensor_state(DriverPid),
	case FloorState of
		0 ->
			drive_up(DriverPid);
		_ ->
			drive_down(DriverPid)
	end.

	