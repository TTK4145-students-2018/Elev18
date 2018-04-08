-module(event_manager).

-compile(export_all).

%-export([start/0]).

% poller of the system, listening for button presses
% and such, sends events to fsm

% TODO:

start() ->
	%{ok, DriverPid} = driver:start(),
    button_poller(0, cab).


sensor_poller() ->
	1.

button_poller(Floor, ButtonType) when Floor >= 0, Floor =< 3 ->
	io:format("halla fra button poller ~n"),
	ButtonState = driver:get_order_button_state(driver, Floor, ButtonType),
	io:format([ButtonState]),
	case ButtonState of
		0 ->
			io:format("Nope ~p~n", [Floor]),
			button_poller(Floor + 1, ButtonType);
		1 ->
			%order_manager ! {add, Floor},
			io:format("Yes ~p~n", [Floor]),
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