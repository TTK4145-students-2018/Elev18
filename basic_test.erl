-module(basic_test).
-export([start/0]).

-define(DELAY, 100).

% This should make the elevator move to the first floor,
% then move between the first and fourth floor

start() ->
	{ok, Driver} = driver:start(),
	init_drive().

init_drive() ->
	FloorState = driver:get_floor_sensor_state(self()),
	case FloorState of
		1 -> 
			drive_up();
		_ ->
			init_drive(down)
	end;

init_drive(Direction) ->
	driver:set_motor_direction(Direction),
	timer:sleep(?DELAY),
	FloorState = driver:get_floor_sensor_state(self()),


drive_up() ->
	driver:set_motor_direction(self(), up).