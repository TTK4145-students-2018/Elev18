-module(fsm).
-export([start/0]).

start() ->
	st_init().

st_init() ->
	io:format("fsm: initializing"),
	receive ev_ground_floor_reached ->
		io:format("fsm: elevator initialized, behold my initial glory"),
		st_idle();
		Other ->
			io:format("fsm_init: received garbage:" ~p~n, [Other]),
			st_init()

	after 10000 ->
		io:format("fsm: init timed out, trying again");
		st_init()
	end.


st_idle() ->
	io:format("fsm: elevator idle"),
	receive 
		order_recieved ->
			st_moving();

		ev_emergency_stop ->
			st_emergency();

		Other ->
			io:format("fsm_idle: received garbage:" ~p~n, [Other]),
			st_idle()
	end.


st_moving() ->
	io:format("moving"),
	receive 
		destination_reached ->
			io:format("fsm: destination reached"),
			st_doors_open();

		ev_emergency_stop ->
			st_emergency();

		Other ->
			io:format("fsm_moving: received garbage:" ~p~n, [Other]),
			st_moving()

	end.

st_doors_open() ->
	io:format("doors opened"),
	receive
		order_recieved ->
			st_moving();

		Other ->
			io:format("fsm_doors_open: received garbage:" ~p~n, [Other]),

	after 2000 ->
		st_idle()
	end.

st_emergency() ->
	io:format("fsm: emergency state activated"),
	receive
		order_recieved ->
			st_moving();

		ev_emergency_stop ->
			st_emergency();

		Other ->
			io:format("fsm_emergency: received garbage:" ~p~n, [Other]),
			st_emergency()

	end.