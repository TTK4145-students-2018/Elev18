-module(network).
-export([start/0]).

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

    %[ID|_T] = hf:flip(IPlist),									% uses last part of IP as ID for elevator
    worldview ! {id, NodeName},
    
 	net_kernel:start([NodeName, longnames, 500]),				% Creates node with heartbeat of 500 milliseconds 
 	erlang:set_cookie(node(), 'Elev18'),

 	spawn(fun() -> listener() end),
 	spawn(fun() -> broadcast() end),
 	register(update_worldviews, spawn(fun() -> update_worldviews([]) end)),
 	register(order_distributor, spawn(fun() -> order_distributor(NodeName) end)),
 	register(order_receiver, spawn(fun() -> order_receiver() end)),
 	spawn(fun() -> moniteur() end),

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
			io:format("Node connected: ~p~n", [Node]), %debug
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
	io:format("Current worldviews: ~p~n", [WorldViews]),
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
			{_, DeadWV} = lists:keysearch(ID, 1, WorldViews),
			DeadOrders = element(4, DeadWV),
			UpdatedViews = lists:keydelete(ID, 1, WorldViews),
			NewViews = reevaluate(DeadOrders, UpdatedViews, OwnID)
		%{order, Order} ->
			%worldview ! {request, wv, update_worldviews},
			%receive {response, wv, WorldView} -> ok end,
			%OwnID = element(1, WorldView),
			%BestID = scheduler:scheduler(WorldViews, Order),
			%case OwnID == BestID of
			%	true ->
			%		order_manager ! {add, Order},
			%		update_worldviews(WorldViews);
			%	false ->
			%		update_worldviews(WorldViews)
			%end
	end,
	order_receiver ! {wv_list, NewViews},
	update_worldviews(NewViews).

order_distributor(Node) ->
	receive
		{new, Order} ->
			send_to_all(order_receiver, {order, Order, Node}),
			receive {order, ack} -> ok end,
			order_receiver ! {order, Order}
	end,
	order_distributor(Node).

order_receiver() ->
	receive
		{wv_list, WorldViews} ->
			order_receiver(WorldViews)
	end.

order_receiver(WorldViews) ->
	receive
		{order, Order, Node} ->
			{order_distributor, Node} ! {order, ack},					%Should this wait for return ack (package loss)
			worldview ! {request, wv, order_receiver},
			receive {response, wv, WorldView} -> ok end,
			OwnID = element(1, WorldView),
			BestID = scheduler:scheduler(WorldViews, Order),
			case OwnID == BestID of
				true ->
					order_manager ! {add, Order},
					order_receiver(WorldViews);
				false ->
					order_receiver(WorldViews)
			end;
		{order, Order} ->
			worldview ! {request, wv, order_receiver},
			receive {response, wv, WorldView} -> ok end,
			OwnID = element(1, WorldView),
			BestID = scheduler:scheduler(WorldViews, Order),
			case OwnID == BestID of
				true ->
					order_manager ! {add, Order},
					order_receiver(WorldViews);
				false ->
					order_receiver(WorldViews)
			end;
		{wv_list, WorldViews} ->
			order_receiver(WorldViews)
	end.



replace_wv(WorldViews, WorldView) ->
	OwnID = element(1, WorldView),
	case lists:keysearch(OwnID, 1, WorldViews) of
		{value, _} ->
			NewViews = lists:keyreplace(OwnID, 1, WorldViews, WorldView);
		false ->
			NewViews = WorldViews ++ [WorldView]
	end,
	NewViews.

send_to_all(Process, Message) ->
	lists:foreach(fun(Node) -> {Process, Node} ! Message end, nodes()).

%reevaluate([], _) ->
%	1.

reevaluate(Orders, WorldViews, OwnID) ->
	% to be run when a node dies, reevaluates all orders from the
	% dead node, and adds them to appropriate node. If id returned from
	% scheduler matches OwnID, the order is added.
	[First|Rest] = Orders,
	case (scheduler:scheduler(WorldViews, First) == OwnID) of
		true ->
			order_manager ! {add, First},
			reevaluate(Rest, WorldViews, OwnID);
		false ->
			reevaluate(Rest, WorldViews, OwnID)
	end.

moniteur() ->
	lists:foreach(fun(Node) -> erlang:monitor_node(Node, true) end, nodes()),
	receive
		{nodedown, Node} ->
			io:format("Node down!! ~n"),
			update_worldviews ! {died, Node},
			moniteur()
	after 1000 ->
		moniteur()
	end.