-module(day1).
-export([part1/0, part2/0]).
-define(FILE_NAME, "input.txt").


part1() -> count(read_input()).

read_input() ->
    case file:read_file(?FILE_NAME) of
	{ok, Bin} -> lists:map(
		       fun(X) -> {Int, _} = string:to_integer(X), Int end,
		       string:lexemes(Bin, "\n")
		      );
	{error, _} -> throw("Unable to open file")
    end.


c2([_], C) -> C;
c2([A,B | T], C) ->
    if
		B > A -> c2([B|T], C+1);
		true -> c2([B|T], C)
    end.

count([]) -> 0;
count([_]) -> 0;
count([H|L]) -> {_, Final} = 
		lists:foldl(
		  fun(X, {Prev, Count}) when X > Prev -> {X, Count+1};
		     (X, {_Prev, Count}) -> {X, Count} end,
		  {H, 0},
		  L), Final.


part2() -> count( three_mesurment(read_input(), [])).

three_mesurment([F,S,T | Other], Acc) -> three_mesurment([S,T | Other], [F+S+T | Acc]);
three_mesurment([_,_], Acc) -> lists:reverse(Acc).
