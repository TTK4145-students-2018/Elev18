-module(network_UDP).
-export([start/0]).

-define(RECV_PORT, 7565).
-define(SEND_PORT, 7479).

start() ->
	init().


init() ->
	os:cmd("epmd -daemon"),
	timer:sleep(100),

	{ok, LongIPlist} = inet:getif(), 							% inet:getif() gives a list of tuples of IPs
	IPlist = tuple_to_list(element(1, hd(LongIPlist))), 		% Header of IPlist is local IP adress 
	NodeName = list_to_atom("elevator@" ++ format_IP(IPlist)), 	% generates a unique nodename

    [ID, _T] = hf:flip(IPlist),									% uses last part of IP as ID for elevator
    %worldview ! {id, ID},
    
 	net_kernel:start([NodeName, longnames, 500]),				% Creates node with heartbeat of 500 milliseconds 
 	erlang:set_cookie(node(), 'Elev18'),

 	node_center ! {network, init_complete}.

listener() ->
 	{ok, ReceiveSocket} = gen_udp:open(?RECV_PORT, [list, {active, false}]),
 	listener(ReceiveSocket).

listener(ReceiveSocket) ->
	{ok, {_Adress, ?SEND_PORT, NodeName}} = gen_udp:recv(ReceiveSocket, 0),
	Node = list_to_atom(NodeName),
	case lists:member(Node, [node()|nodes()]) of
		true ->
			listen(ReceiveSocket);
		false ->
			net_adm:ping(Node), % ping node to create a connection
			io:format("Node connected: ~p~n", [Node]), %debug
			listen(ReceiveSocket)
		end.



broadcast() ->
	{ok, SendSocket} = gen_udp:open(?SEND_PORT, [list, {active, true}, {broadcast, true}]),
	broadcast(SendSocket).

broadcast(SendSocket) ->
	ok = gen_udp:send(SendSocket, {255,255,255,255}, ?RECEIVE_PORT, atom_to_list(node())),
	timer:sleep(2000),
	broadcast(SendSocket).



% Formats IP from a tuple to a string.
format_IP(IPlist) ->
	[_Head | IP] = lists:flatmap(fun(X) -> ['.', X] end, IPlist),
	lists:concat(IP).

update_worldview(Worldview, WorldviewList) ->
	lists:foreach(fun (Node) -> Node ! {update_worldview, Worldview} end, nodes()),
	receive {update_worldview, OtherWorldview} ->
		update_worldview(Worldview, WorldviewList ++ [OtherWorldview]),
	after 3000 ->
		update_worldview(Worldview, WorldviewList)
