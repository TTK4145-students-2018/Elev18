-module(node_center).
-export([start/0]).

% central module which starts all required processes
% and ensures everything is running

%TODO

start() ->
	FSM = fsm:start(Driver) %%no idea if this is right
