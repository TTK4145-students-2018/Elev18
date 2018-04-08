-module(node_center).
-export([start/0]).

% central module which starts all required processes
% and ensures everything is running

%TODO

start() ->
	%register(fsm, fsm:start()),
	%register(order_manager, order_manager:start()),
	{ok, DriverPid} = driver:start(),
	register(driver, DriverPid),
	register(event_manager, spawn(fun event_manager:start/0)).
