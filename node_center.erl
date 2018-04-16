-module(node_center).
-export([start/0]).

% central module which starts all required processes
% and ensures everything is initialized in the proper
% order.
% all processes in a node are as follows:
% node_center, driver, worldview, network, event_manager
% order_manager, fsm, update_worldviews, order_distributor
% order_receiver

start() ->
	register(node_center, self()),
	{ok, DriverPid} = driver:start(),
	register(driver, DriverPid),
	register(worldview, spawn(fun worldview:start/0)),
	register(network, spawn(fun network:start/0)),

	receive {network, init_complete} -> ok end,
			
	register(event_manager, spawn(fun event_manager:start/0)),
	register(order_manager, spawn(fun order_manager:start/0)),
	register(fsm, spawn(fun fsm:start/0)).
