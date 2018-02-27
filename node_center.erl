-module(node_center).
-export([start/0]).

start() ->
	FSM = spawn(fun() -> fsm:start()end).
