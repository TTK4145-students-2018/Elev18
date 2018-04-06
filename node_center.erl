-module(node_center).
-export([start/0]).

% central module which starts all required processes
% and ensures everything is running

%TODO

start() ->
	register(fsm, fsm:start()),
	register(order_manager, order_manager:start()),
	register(driver, driver:start()),
	register(event_manager, event_manager:start()).