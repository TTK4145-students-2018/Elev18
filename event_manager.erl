-module(event_manager).

-compile(export_all).

-define(DELAY, 100).

%-export([start/0]).

% Poller of our system, currently listening for order-button-presses of all
% types in all floors, as well as floor sensor. Sends information to fsm and
% worldview.

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
			case SensorState =:= LastState of
				true ->
					floor_sensor_poller(SensorState);
				false ->
					fsm ! {ev_floor_reached, SensorState},
					worldview ! {floor, SensorState},
					driver:set_floor_indicator(driver, SensorState),
					%io:format("SensorState: ~p~n", [SensorState]),
					timer:sleep(?DELAY),
					floor_sensor_poller(SensorState)
			end
	end.



button_poller(4, cab) ->
	button_poller(0, cab);

button_poller(4, hall_down) ->
	button_poller(1, hall_down);

button_poller(3, hall_up) ->
	button_poller(0, hall_up);

button_poller(Floor, ButtonType) ->
	ButtonState = driver:get_order_button_state(driver, Floor, ButtonType),
	case ButtonState of
		0 ->
			%timer:sleep(?DELAY),
			button_poller(Floor + 1, ButtonType);
		1 ->
			%NewOrder = {order, Floor, ButtonType},
			%network ! NewOrder,
			NewOrder = {Floor, ButtonType},
			%order_manager ! {add, NewOrder},
			order_distributor ! {new, NewOrder},
			timer:sleep(?DELAY),
			button_poller(Floor + 1, ButtonType)
	end.


event_handler() ->
	1.

reset_button(4, cab) ->
	1;

reset_button(4, hall_down) ->
	1;

reset_button(3, hall_up) ->
	1;

reset_button(Floor, ButtonType) ->
	driver:set_order_button_light(driver, ButtonType, Floor, off),
	reset_button(Floor + 1, ButtonType).

reset_all() ->
	reset_button(0, cab),
	reset_button(1, hall_down),
	reset_button(0, hall_up).

