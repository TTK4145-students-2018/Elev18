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
 	erlang:set_cookie(node(), 'Elev18').


 %listener() ->
% 	{ok, ReceiveSocket} = gen_udp:open(?RECV_PORT, [list, {active, false}]),
% 	listener(ReceiveSocket).

% Formats IP from a tuple to a string.
format_IP(IPlist) ->
	[_Head | IP] = lists:flatmap(fun(X) -> ['.', X] end, IPlist),
	lists:concat(IP).