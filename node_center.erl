-module(node_center).
-export([start/0]).

% central module which starts all required processes
% and ensures everything is running

%TODO

start() ->
	register(node_center, self()),
	%{ok, Port} = io:read("Elevator port: ", "~d"),
	{ok, DriverPid} = driver:start(),							%{127,0,0,1}, 011095
	register(driver, DriverPid),
	register(worldview, spawn(fun worldview:start/0)),
	register(network, spawn(fun network:start/0)),
	register(event_manager, spawn(fun event_manager:start/0)),

	
	receive {network, init_complete} -> ok end,
			
	register(order_manager, spawn(fun order_manager:start/0)),
	register(fsm, spawn(fun fsm:start/0)).
