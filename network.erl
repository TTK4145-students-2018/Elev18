-module(network).
-export([start/0]).


% Network module uses distibuted Erlang (node cluster)
% to communicate with other nodes


% TO DO
% 
% Establish connection with other nodes
% Distribute worldview
% Distribute orders

start() ->
	init().


init() ->
	os:cmd("epmd -daemon"),

	{ok, LongIPlist} = inet:getif(), 							% inet:getif() gives a list of tuples of IPs
	IPlist = tuple_to_list(element(1, hd(LongIPlist))), 							% Header of IPlist is local IP adress 
	NodeName = list_to_atom("elevator@" ++ format_IP(IPlist)), 	% generates a unique nodename

    [ID|_T] = hf:flip(IPlist),									% uses last part of IP as ID for elevator
    worldview ! {id, ID},

 	net_kernel:start([NodeName, longnames, 500]),				% Creates node with heartbeat of 500 milliseconds 
 	erlang:set_cookie(node(), 'Elev18').
 	%register(shell, self()).

	%net_adm:world(verbose).

	%Hosts = net_adm:host_file(),
	%connect_nodes(Hosts),



connect_nodes(Hosts) ->
	[IP|NewHosts] = Hosts,
	NodeName = "elevator@" ++ IP,

	net_kernel:connect_node(NodeName),

	connect_nodes(NewHosts).

%network() ->
%	
%	receive {id, ID, from, OtherShell} ->
%		register(list_to_atom("worldview" ++ ID )

%make_node_list() ->
	

%update_worldviews() ->
%	receive {, Worldview, OtherShell, ID} ->






% Formats IP from a tuple to a string.
format_IP(IPlist) ->
	[_Head | IP] = lists:flatmap(fun(X) -> ['.', X] end, IPlist),
	lists:concat(IP).
