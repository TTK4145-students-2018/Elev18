-module(event_manager).

-compile(export_all).

-define(DELAY, 100).

%-export([start/0]).

% poller of the system, listening for button presses
% and such, sends events to fsm

% TODO:

start() ->
    spawn(fun() -> floor_sensor_poller(1) end),
	spawn(fun() -> button_poller(0, cab) end),
	spawn(fun() -> button_poller(1, hall_down) end),
	spawn(fun() -> button_poller(0, hall_up) end).

floor_sensor_poller(LastState) ->
	SensorState = driver:get_floor_sensor_state(driver),
	%io:format("SensorState: ~p~n", [SensorState]),
	case SensorState of
		between_floors ->
			case SensorState =:= LastState of
				true -> 
					floor_sensor_poller(SensorState);
				false ->
					worldview ! {floor, between_floors},
					%timer:sleep(?DELAY),
					floor_sensor_poller(SensorState)
			end;
		_ ->
			fsm ! {ev_floor_reached, SensorState},
			worldview ! {floor, SensorState},
			driver:set_floor_indicator(driver, SensorState),
			%io:format("SensorState: ~p~n", [SensorState]),
			timer:sleep(?DELAY),
			floor_sensor_poller(SensorState)
	end.



button_poller(4, cab) ->
	button_poller(0, cab);

button_poller(4, hall_down) ->
	button_poller(1, hall_down);

button_poller(3, hall_up) ->
	button_poller(0, hall_up);

button_poller(Floor, ButtonType) ->
	%io:format("halla fra button poller ~n"),
	ButtonState = driver:get_order_button_state(driver, Floor, ButtonType),
	case ButtonState of
		0 ->
			%io:format("Nope ~p~n", [Floor]),
			%timer:sleep(?DELAY),
			button_poller(Floor + 1, ButtonType);
		1 ->
			%NewOrder = {order, Floor, ButtonType},
			%network ! NewOrder,
			%io:format("Button pressed: ~p~n", [Floor]),
			order_manager ! {add, Floor},
			timer:sleep(?DELAY),
			button_poller(Floor + 1, ButtonType)
	end.


event_handler() ->
	1.

