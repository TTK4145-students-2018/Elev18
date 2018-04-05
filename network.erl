-module(network).
-export([net_init/0]).



% Network module uses distibuted Erlang (node cluster)
% to communicate with other nodes


% TO DO
% 
% Establish connection with other nodes
% Distribute worldview
% Distribute orders

net_init() ->
	os:cmd("epmd -daemon"),

	{ok, LongIPlist} = inet:getif(), 							% inet:getif() gives a list of tuples of IPs
	IPlist = tuple_to_list(element(1, hd(LongIPlist))), 							% Header of IPlist is local IP adress 

	NodeName = list_to_atom("elevator@" ++ format_IP(IPtuple)), 	% generates a unique nodename

    %[IP|_T] = hf:flip(IP),

 	net_kernel:start([NodeName, longnames, 500]),			% Creates node with heartbeat of 500 milliseconds 
 	erlang:set_cookie(node(), 'glue'),

	Hosts = net_adm:host_file(),






% Formats IP from a tuple to a string - from Sivertba's ErlangHeis
format_IP(IPlist) ->
	[_Head | IP] = lists:flatmap(fun(X) -> ['.', X] end, IPlist),
	lists:concat(IP).