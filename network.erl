-module(network).
-export(start/0).

%Network module uses distibuted Erlang (node cluster)
%to communicate with other nodes


%TO DO
%Establish connection with other nodes
%Distribute worldview
%Distribute orders

start ->
	net_init().

net_init ->
	