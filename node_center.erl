-module(node_center).
-export([start/0]).

% central module which starts all required processes
% and ensures everything is running

%TODO

start() ->
	register(fsm, spawn(fun fsm:start/1)),
	register(order_manager, spawn(fun order_manager:start/0)),
	{ok, DriverPid} = driver:start(),
	register(driver, DriverPid),
	register(event_manager, spawn(fun event_manager:start/0)),
	spawn(fun basic_test:init_drive/0).
