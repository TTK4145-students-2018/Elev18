-module(fsm).
-export([start/0]).

start() ->
	st_init().

st_init() ->
	io:format("initializing").
	fsm_handler ! {initializing},
	receive ev_ground_floor_reached ->
		fsm_handler ! {init_complete}
		io:format("elevator initialized, behold my initial glory").
	end,

	st_idle().

st_idle() ->
	io:format("elevator idle").
	fsm_handler ! {idle},
	receive 
		ev_new_call ->
			io:format("call recieved"),
			st_idle();

		order_recieved ->
			st_moving();

		ev_emergency_stop ->
			st_emergency()

	end.


st_moving() ->
	io:format("moving").
	fsm_handler ! {moving},
	receive 
		destination_reached ->
			io:format("destination reached"),
			st_doors_open();

		ev_emergency_stop ->
			st_emergency()

	end.

st_doors_open() ->
	io:format("doors opened").
	fsm_handler ! {doors_open}.
	receive
		order_recieved ->
			st_moving()
	end.

st_emergency() ->
	io:format("emergency state activated").
	fsm_handler ! {emergency}.
	receive
		order_recieved ->
			st_moving();

	end.
