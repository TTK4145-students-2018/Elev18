-module(event_manager).

-compile(export_all).

%-export([start/0]).

% poller of the system, listening for button presses
% and such, sends events to fsm

% TODO:

start() ->
	%{ok, DriverPid} = driver:start(),
    button_poller(0, cab).
    %floor_sensor_poller().


floor_sensor_poller() ->
	io:format("Hello from sensor poller ~n"),
	SensorState = driver:get_floor_sensor_state(self()),
	io:format("SensorState: ~p~n", [SensorState]),
	receive
		{reply, State, Socket} ->
			io:format("State: ~p~n", [State]),
			io:format("Socket: ~p~n", [Socket]),
			floor_sensor_poller()
	end.


button_poller(4, ButtonType) ->
	button_poller(0, ButtonType);

button_poller(Floor, ButtonType) ->
	%io:format("halla fra button poller ~n"),
	ButtonState = driver:get_order_button_state(driver, Floor, ButtonType),
	case ButtonState of
		0 ->
			%io:format("Nope ~p~n", [Floor]),
			button_poller(Floor + 1, ButtonType);
		1 ->
			io:format("Button pressed: ~p~n", [Floor]),
			%order_manager ! {add, Floor},
			button_poller(Floor + 1, ButtonType)
	end.

button_poller(DriverPid, Floor, ButtonType) when Floor >= 0, Floor =< 3 ->
	ButtonState = driver:get_order_button_state(DriverPid, Floor, ButtonType),
	case ButtonState of
		0 ->
			io:format("Nope ~p~n", [Floor]),
			button_poller(DriverPid, Floor + 1, ButtonType);
		1 ->
			%order_manager ! {add, Floor},
			io:format("Yes ~p~n", [Floor]),
			button_poller(DriverPid, Floor + 1, ButtonType)
	end.



event_handler() ->
	1.

ev_button_pressed() ->
	1.