-module(node_center).
-export([start/0]).

% central module which starts all required processes
% and ensures everything is running

%TODO

start() ->
	register(node_center, self()),
	{ok, DriverPid} = driver:start(),
	register(driver, DriverPid),
	register(network, spawn(fun network:start/0)),
	receive {network, init_complete} -> ok end,


	register(event_manager, spawn(fun event_manager:start/0)),
	register(worldview, spawn(fun worldview:start/0)),

	register(order_manager, spawn(fun order_manager:start/0)),
	register(fsm, spawn(fun fsm:start/0)).
	


	%spawn(fun basic_test:init_drive/0).
