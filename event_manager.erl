-module(event_manager).
-export([start/1]).

% poller of the system, listening for button presses
% and such, sends events to fsm

% TODO:

start(FSM) ->
	spawn(fun() -> poller(FSM) end).
	EH = spawn(fun() -> event_handler(FSM) end).

poller(FSM) ->
	end.

event_handler(FSM) ->
	end.