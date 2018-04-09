-module(worldview).
-compile(export_all).

%WorldView = {ID, state, last_floor, orders, direction}
%ID from the get-go
%state from fsm
%last_floor from fsm
%orders from order_manager
%direction from fsm

start() ->
	receive {id, ID} -> ok end,
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

		{request, wv, Pid} ->
			Pid ! {response, wv, WorldView};

		{request, direction, Pid} ->
			Pid ! {response, direction, get_direction(WorldView)}
	end,
	world(WorldView).

get_direction(WorldView) ->
	% NOTE: doesn't react well to being called between floors. i.e. if it
	% has last floor registered the same as next floor, it will return down
	% regardless of actual position relative to destination.
	
	LastFloor = element(3, WorldView),
	[NextFloor|Rest] = element(4, WorldView),
	case LastFloor < NextFloor of
		true -> up;
		false -> down
	end.