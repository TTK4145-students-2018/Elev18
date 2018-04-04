-module(network).
-export([net_init/0]).

% Network module uses distibuted Erlang (node cluster)
% to communicate with other nodes


% TO DO
% Establish connection with other nodes
% Distribute worldview
% Distribute orders

net_init() ->
	os:cmd("epmd -daemon"),

	{ok, IPlist} = inet:getif(), 							% inet:getif() gives a list of tuples of IPs
	IP = element(1, hd(IPlist)), 							% Header of IPlist is local IP adress
	NodeName = list_to_atom("elevator@" ++ format_IP(IP)), 	% generates a unique nodename

 	net_kernel:start([NodeName, longnames, 500]),			% Creates node with heartbeat of 500 milliseconds 
 	erlang:set_cookie(node(), 'glue'),

	Hosts = net_adm:host_file(),
	Hosts.






% Formats IP from a tuple to a string - from Sivertba's ErlangHeis
format_IP(IPtuple) ->
	[_Head | IPlist] = lists:flatmap(fun(X) -> ['.', X] end, tuple_to_list(IPtuple)),
	lists:concat(IPlist).