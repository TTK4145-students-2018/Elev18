-module(event_manager).
-export([start/1]).

% poller of the system, listening for button presses
% and such, sends events to fsm

% TODO:

start(FSM) ->
	spawn(fun() -> poller(FSM) end).
	EH = spawn(fun() -> event_handler(FSM) end).

sensor_poller(FSM) ->
	1.

button_poller(FSM) ->
	1.

event_handler(FSM) ->
	1.