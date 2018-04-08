-module(worldview).
-compile(export_all).

%WorldView = {ID, state, last_floor, orders, direction}
%ID from the get-go
%state from fsm
%last_floor from fsm
%orders from order_manager
%direction from fsm

start(ID) ->
	WorldView = {ID, idle, 0, [], stop},
	world(WorldView).

world(WorldView) ->
	io:format("current worldview: ~p~n", [tuple_to_list(WorldView)]),
	receive
		{state, State} ->
			NewView = setelement(2, WorldView, State),
			world(NewView);

		{floor, LastFloor} ->
			NewView = setelement(3, WorldView, LastFloor),
			world(NewView);

		{orders, Orders} ->
			NewView = setelement(4, WorldView, Orders),
			world(NewView);

		{direction, Dir} ->
			NewView = setelement(5, WorldView, Dir),
			world(NewView);

		{request, Pid} ->
			Pid ! {WorldView}
	end.