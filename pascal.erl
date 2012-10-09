-module (pascal).

-export([pascal/2, demo/0]).

generate_next_line(NextLine, [_] ) -> [1 | lists:reverse([1 | NextLine])];
generate_next_line(NextLine, [H | [H2|T]] ) ->
  generate_next_line(
    [H + H2 | NextLine], [H2|T]
  ).

generate_next_line(List) -> generate_next_line([], List).


get_line_of_triangle(RequiredLineNumber, CurrentLineNumber, Line)
  when RequiredLineNumber =:= CurrentLineNumber  -> Line;
get_line_of_triangle(RequiredLineNumber, CurrentLineNumber, Line)
  -> get_line_of_triangle(RequiredLineNumber, CurrentLineNumber + 1, generate_next_line(Line)).


get_line_of_triangle(LineNumber) ->
  get_line_of_triangle(LineNumber, 0, [1]).


pascal(ElementIndex, LineNumber) ->
  lists:nth(ElementIndex+1, get_line_of_triangle(LineNumber)).

demo() ->
  [io:format("~p~n", [get_line_of_triangle(X)]) || X <- lists:seq(0, 6)],
  ok
  .