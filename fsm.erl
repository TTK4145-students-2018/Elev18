-module(fsm).
-export([start/0]).

% the finite state machine of our system
% is to have control over the internal
% state of the node it runs on,
% based on events received from the driver

%TODO:

start() ->
	st_init().

st_init() ->
	io:format("fsm: initializing ~n"),
	receive ev_ground_floor_reached ->
		io:format("fsm: elevator initialized, behold my initial glory ~n"),
		st_idle();
		Other ->
			io:format("fsm_init: received garbage: ~p~n", [Other]),
			st_init()

	after 10000 ->
		io:format("fsm: init timed out, trying again ~n"),
		st_init()
	end.


st_idle() ->
	io:format("fsm: elevator idle ~n"),
	receive 
		order_received ->
			st_moving();

		ev_emergency_stop ->
			st_emergency();

		Other ->
			io:format("fsm_idle: received garbage: ~p~n", [Other]),
			st_idle()
	end.


st_moving() ->
	io:format("fsm: moving ~n"),
	receive 
		destination_reached ->
			io:format("fsm: destination reached ~n"),
			st_doors_open();

		ev_emergency_stop ->
			st_emergency();

		Other ->
			io:format("fsm_moving: received garbage: ~p~n", [Other]),
			st_moving()

	end.

st_doors_open() ->
	io:format("doors opened ~n"),
	receive
		order_received ->
			st_moving();

		Other ->
			io:format("fsm_doors_open: received garbage: ~p~n", [Other])

	after 2000 ->
		st_idle()
	end.

st_emergency() ->
	io:format("fsm: emergency state activated ~n"),
	receive
		order_received ->
			st_moving();

		ev_emergency_stop ->
			st_emergency();

		Other ->
			io:format("fsm_emergency: received garbage: ~p~n", [Other]),
			st_emergency()

	end.