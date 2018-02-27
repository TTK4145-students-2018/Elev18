-module(fsm).
-export([start/0]).

start() ->
	st_init().

st_init() ->
	io:format("fsm: initializing").
	fsm_handler ! {initializing},
	receive ev_ground_floor_reached ->
		fsm_handler ! {init_complete}
		io:format("fsm: elevator initialized, behold my initial glory").
	end,

	st_idle().

st_idle() ->
	io:format("fsm: elevator idle").
	fsm_handler ! {idle},
	receive 
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
			io:format("fsm: destination reached"),
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

	after 2000 ->
		fsm_handler ! {doors_close}.
		st_idle().
	end.

st_emergency() ->
	io:format("fsm: emergency state activated").
	fsm_handler ! {emergency}.
	receive
		order_recieved ->
			st_moving();

	end.
