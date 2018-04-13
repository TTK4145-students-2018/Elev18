-module(network).
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
	NodeName = list_to_atom("elevator@" ++ hf:format_IP(IPlist)), 	% generates a unique nodename

    [ID|_T] = hf:flip(IPlist),									% uses last part of IP as ID for elevator
    worldview ! {id, ID},
    
 	net_kernel:start([NodeName, longnames, 500]),				% Creates node with heartbeat of 500 milliseconds 
 	erlang:set_cookie(node(), 'Elev18'),

 	%node_center ! {network, init_complete}.
 	spawn(fun() -> listener() end),
 	spawn(fun() -> broadcast() end),
 	spawn(fun() -> distribute_worldview() end),
 	spawn(fun() -> fetch_worldview([]) end).
 	

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



distribute_worldview() ->
	%worldview ! {request, wv, self()},
 	receive {wv, Worldview} ->
 		io:format("Local Worldview received!"),
 		send_to_all(fetch_worldview, {wv, Worldview}),
 		distribute_worldview()
 	after 1000 ->
 		distribute_worldview()
 	end.

fetch_worldview(WorldviewList) ->
	receive {wv, Worldview} ->
		%list_replace(WorldviewList, Worldview)
		io:format("Received external worldview")
	end,
	fetch_worldview(WorldviewList).



send_to_all(Process, Message) ->
	lists:foreach(fun(Node) -> {Process, Node} ! Message end, nodes()).

