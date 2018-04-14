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
	receive {network, init_complete} -> ok end,
	world(WorldView).


world(WorldView) ->
	update_worldviews ! {self, wv, WorldView},
	io:format("Worldview updated~n"),
	%io:format("current worldview: ~p~n", [WorldView]),
	receive
		{state, State} ->
			NewView = setelement(2, WorldView, State),
			world(NewView);

		{floor, between_floors} ->
			case is_float(element(3, WorldView)) of
				true ->
					world(WorldView);
				false ->
					case element(5, WorldView) of
						stop ->
							world(WorldView);
						up ->
							NewView = setelement(3, WorldView, element(3, WorldView) + 0.5),
							world(NewView);
						down ->
							NewView = setelement(3, WorldView, element(3, WorldView) - 0.5),
							world(NewView)
					end
			end;

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
	LastFloor = element(3, WorldView),
	case is_empty(element(4, WorldView)) of
		true -> stop;
		false ->
			[NextOrder|_] = element(4, WorldView),
			NextFloor = element(1, NextOrder),
			case LastFloor == NextFloor of
				true -> stop;
				false ->
					case LastFloor < NextFloor of
						true -> up;
						false -> down
					end
			end
	end.

is_empty([]) ->
	true;
is_empty(_) ->
	false.