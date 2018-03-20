-module(event_manager).
-export([start/1]).

% poller of the system, listening for button presses
% and such, sends events to fsm

% TODO:

start(FSM) ->
	spawn(fun() -> poller(FSM) end).

poller(FSM) ->
	fdsfdsa.