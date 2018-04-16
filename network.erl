-module(network).
-export([start/0, send_to_all/2]).

-define(RECV_PORT, 7565).
-define(SEND_PORT, 7479).

-define(TIMEOUT, 100).

start() ->
	init().

init() ->
	os:cmd("epmd -daemon"),
	timer:sleep(100),

	{ok, LongIPlist} = inet:getif(), 							% inet:getif() gives a list of tuples of IPs
	IPlist = tuple_to_list(element(1, hd(LongIPlist))), 		% Header of IPlist is local IP adress 
	NodeName = list_to_atom("elevator@" ++ hf:format_IP(IPlist)), 	% generates a unique nodename

    worldview ! {id, NodeName},
 	net_kernel:start([NodeName, longnames, 500]),				% Creates node with heartbeat of 500 milliseconds 
 	erlang:set_cookie(node(), 'Elev18'),

 	spawn(fun() -> listener() end),
 	spawn(fun() -> broadcast() end),
 	register(update_worldviews, spawn(fun() -> update_worldviews([]) end)),
 	register(order_distributor, spawn(fun() -> order_distributor(NodeName) end)),
 	register(order_receiver, spawn(fun() -> order_receiver() end)),

 	worldview ! {network, init_complete},
 	node_center ! {network, init_complete}.


listener() ->
 	{ok, ReceiveSocket} = gen_udp:open(?RECV_PORT, [list, {active, false}]),
 	listener(ReceiveSocket).

listener(ReceiveSocket) ->
	{ok, {_Adress, ?SEND_PORT, NodeName}} = gen_udp:recv(ReceiveSocket, 0),
	Node = list_to_atom(NodeName),
	case lists:member(Node, [node()|nodes()]) of
		true ->
			listener(ReceiveSocket);
		false ->
			net_adm:ping(Node), % ping node to create a connection
			io:format("Node connected: ~p~n", [Node]),
			spawn(fun() -> node_watcher(Node) end),
			listener(ReceiveSocket)
	end.


broadcast() ->
	{ok, SendSocket} = gen_udp:open(?SEND_PORT, [list, {active, true}, {broadcast, true}]),
	broadcast(SendSocket).

broadcast(SendSocket) ->
	ok = gen_udp:send(SendSocket, {255,255,255,255}, ?RECV_PORT, atom_to_list(node())),
	timer:sleep(2000),
	broadcast(SendSocket).


update_worldviews(WorldViews) ->
	% keeps track of the worldviews of all nodes in the network. Kept
	% in a changing list WorldViews. This is in case one node dies,
	% so that the remaining nodes can redistribute the dead node's 
	% unfinished orders.

	receive 
		{self, wv, WorldView} ->
			send_to_all(update_worldviews, {other, wv, WorldView}),
			NewViews = replace_wv(WorldViews, WorldView);

		{other, wv, WorldView} ->
			NewViews = replace_wv(WorldViews, WorldView);

		{died, ID} ->
			worldview ! {request, wv, update_worldviews},
			receive {response, wv, WorldView} -> ok end,
			OwnID = element(1, WorldView),
			io:format("Node died: ~p~n", [ID]),
			io:format("distributing orders ~n"),
			Died = lists:keysearch(ID, 1, WorldViews),
			case Died == false of
				true ->
					NewViews = WorldViews;
				false ->
					{_, DeadWV} = Died,
					DeadOrders = element(4, DeadWV),
					UpdatedViews = lists:keydelete(ID, 1, WorldViews),
					NewViews = reevaluate(DeadOrders, UpdatedViews, OwnID)
			end;

		{request, wvs, Pid} ->
			NewViews = WorldViews,
			Pid ! {response, WorldViews}			
	end,
	update_worldviews(NewViews).

replace_wv(WorldViews, WorldView) ->
	OwnID = element(1, WorldView),
	case lists:keysearch(OwnID, 1, WorldViews) of
		{value, _} ->
			NewViews = lists:keyreplace(OwnID, 1, WorldViews, WorldView);
		false ->
			NewViews = WorldViews ++ [WorldView]
	end,
	NewViews.


order_distributor(Node) ->
	% receives locally produced orders, and sends to the other nodes.

	receive
		{new, Order} ->
			send_to_all(order_receiver, {order, Order, Node}),
			order_receiver ! {order, Order}
	end,
	order_distributor(Node).

order_receiver() ->
	% receives locally and externally produced orders and uses worldviews
	% and scheduler to determine if it is to send said order to order_manager
	% where it is saved for local completion.

	receive
		{order, remove, Order} ->
			driver:set_order_button_light(driver, element(2, Order), element(1, Order), off),
			order_receiver();

		{order, Order, Node} ->
			update_worldviews ! {request, wvs, self()},
			receive {response, WorldViews} -> ok end,
			worldview ! {request, wv, order_receiver},
			receive {response, wv, WorldView} -> ok end,
			OwnID = element(1, WorldView),
			BestID = scheduler:scheduler(WorldViews, Order),
			case OwnID == BestID of
				true ->
					order_manager ! {add, Order},
					driver:set_order_button_light(driver, element(2, Order), element(1, Order), on),
					order_receiver();
				false ->
					driver:set_order_button_light(driver, element(2, Order), element(1, Order), on),
					order_receiver()
			end;
			
		{order, Order} ->
			update_worldviews ! {request, wvs, self()},
			receive {response, WorldViews} -> ok end,
			worldview ! {request, wv, order_receiver},
			receive {response, wv, WorldView} -> ok end,
			OwnID = element(1, WorldView),
			BestID = scheduler:scheduler(WorldViews, Order),
			case OwnID == BestID of
				true ->
					order_manager ! {add, Order},
					driver:set_order_button_light(driver, element(2, Order), element(1, Order), on),
					order_receiver();
				false ->
					driver:set_order_button_light(driver, element(2, Order), element(1, Order), on),
					order_receiver()
			end
	end.


send_to_all(Process, Message) ->
	% sends Message to a specified process Process in every node in
	% nodes().

	lists:foreach(fun(Node) -> {Process, Node} ! Message end, nodes()).


reevaluate([], WorldViews, OwnID) ->
	WorldViews;

reevaluate(Orders, WorldViews, OwnID) ->
	% to be run when a node dies, reevaluates all orders from the
	% dead node, and adds them to appropriate node. If ID returned from
	% scheduler matches OwnID, the order is added. It discards all
	% cab orders, as these aren't transferrable.

	[First|Rest] = Orders,
	case element(2, First) of
		cab ->
			reevaluate(Rest, WorldViews, OwnID);
		_ ->
			case (scheduler:scheduler(WorldViews, First) == OwnID) of
				true ->
					order_manager ! {add, First},
					reevaluate(Rest, WorldViews, OwnID);
				false ->
					reevaluate(Rest, WorldViews, OwnID)
			end
	end.


node_watcher(Node) ->
	% spawned in listener(). Monitors a single node, and sends a message
	% to update_worldviews() if it no longer hears said node's heartbeat.
	io:format("I will now monitor: ~p~n", [Node]),
	erlang:monitor_node(Node, true),
	receive
		{nodedown, Node} ->
			io:format("Node down!! ~n"),
			update_worldviews ! {died, Node}
	end.