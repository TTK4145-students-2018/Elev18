-module(event_manager).

-compile(export_all).

%-export([start/0]).

% poller of the system, listening for button presses
% and such, sends events to fsm

% First round of dev: FSMPid is not used as functions are tested locally

% TODO:

start() ->
    {ok, DriverPid} = driver:start(),
    button_poller(DriverPid).


sensor_poller(DriverPid) ->
	1.

button_poller(DriverPid, Floor) when Floor >= 0, Floor <= 3 ->
	ButtonState = driver:get_order_button_state(DriverPid, Floor, cab),
	case ButtonState of
		0 ->
			button_poller(DriverPid, Floor + 1);
		1 ->
			


event_handler(DriverPid) ->
	1.

ev_button_pressed(DriverPid) ->
	1.