-module(game_of_life).
-export([run/0]).
% -compile(export_all).


make_universe() -> {universe, sets:new()}.

set_coordinate(Coordinates = {_X, _Y}, {universe, LivingCells}) ->
    {universe, sets:add_element(Coordinates, LivingCells)}.

populate_universe(Universe = {universe, _}, CoordinatesList) ->
    lists:foldl(fun set_coordinate/2, Universe, CoordinatesList ).


neighbours_around({X, Y}) ->
  [{Coord1, Coord2} || Coord1 <- [X-1, X, X+1], Coord2 <- [Y-1, Y, Y+1], {Coord1, Coord2} =/= {X,Y}].

is_alive_at({universe, LivingCells}, Coordinates = {_X, _Y}) ->
  sets:is_element(Coordinates, LivingCells).

count_neighbours_around(Universe = {universe, _}, Coordinates = {_X, _Y}) ->
    length(
      lists:filter(
        fun(Coords) -> is_alive_at(Universe, Coords) end,
        neighbours_around(Coordinates)
      )
    ).

dump_universe({universe, LivingCells}) -> lists:foreach(
  fun(X) -> io:format("~p~n", [X]) end, sets:to_list(LivingCells)
).

tick(U = {universe, LivingCells}) ->
  {LiveCreaturesRecalculated, CalculatedCoordinates} = lists:foldl(
    fun(Coordinates, {SetForNewLiveCreatures, SetWithCalculatedCoordinates}) ->

      NewSetWithCalculatedCoordinates = sets:add_element(Coordinates, SetWithCalculatedCoordinates),

      NumberOfNeigthbours = count_neighbours_around(U, Coordinates),

      if
        NumberOfNeigthbours =:= 2; NumberOfNeigthbours =:= 3  ->
          {sets:add_element(Coordinates, SetForNewLiveCreatures), NewSetWithCalculatedCoordinates};
        true -> {SetForNewLiveCreatures, NewSetWithCalculatedCoordinates}
      end
    end,
    {sets:new(), sets:new()},
    sets:to_list(LivingCells)
  ),


  AllNeighbouringCells = sets:from_list(
    lists:flatten(
      lists:map(
        fun neighbours_around/1 ,
        sets:to_list(LivingCells)
      )
    )
  ),


  YetUnprocessedNeighbouringCells = lists:filter(
    fun(Coordinates) ->
      not sets:is_element(Coordinates, CalculatedCoordinates)
    end,
    sets:to_list(AllNeighbouringCells)),

  NewLivingCells = lists:foldl(
    fun(Coordinates, Acc) ->

      NumberOfLivingNeighbours = count_neighbours_around(U, Coordinates),

      if
        NumberOfLivingNeighbours =:= 3 ->
          sets:add_element(Coordinates, Acc);
        true -> Acc
      end
    end,
    LiveCreaturesRecalculated,
    YetUnprocessedNeighbouringCells
  ),

  {universe, NewLivingCells}.


run() ->
  Universe3 = populate_universe(make_universe(), [{1,2}, {1,3}, {1,4}]),

  dump_universe(Universe3),
  io:format("------~n", []),

  Universe4 = tick(Universe3),
  dump_universe(Universe4),
  io:format("------~n", []),

  Universe5 = tick(Universe4),
  dump_universe(Universe5),
  io:format("------~n", [])
  .
