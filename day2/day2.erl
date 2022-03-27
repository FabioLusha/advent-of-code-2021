-module(day2).
-compile(export_all).

-define(FILE_NAME, "input.txt").

read_input() ->
    case file:read_file(?FILE_NAME) of
        {ok, Bin} -> Bin;
        {error, Why} -> throw(["Couldn't read the file", Why])
    end.

atomize(Input) -> 
    lists:map(
        fun(X) -> 
            [Key, Val] = string:lexemes(string:trim(X), " "),
            {Int, _Rest} = string:to_integer(Val),
            {binary_to_atom(Key), Int} end,
        string:lexemes(Input, "\n")
    ).

journey([], A = {_Hz, _Depth}) -> A;
journey([H|T], {Hz, Depth}) ->
    case H of
        {forward, Val} -> journey(T, {Hz+Val, Depth});
        {up, Val} -> journey(T, {Hz, Depth-Val});
        {down, Val} -> journey(T, {Hz, Depth+Val})
    end.

part1() -> In = read_input(),
           InA = atomize(In),
           {Hz, Depth} = journey(InA, {0,0}),
           Hz*Depth.

journey2([], A = {_Hz, _Depth}, _) -> A;
journey2([H|T], {Hz, Depth}, Aim) ->
    case H of
        {forward, Val} -> journey2(T, {Hz+Val, Depth+Aim*Val}, Aim);
        {up, Val} -> journey2(T, {Hz, Depth}, Aim-Val);
        {down, Val} -> journey2(T, {Hz, Depth}, Aim+Val)
    end.

part2() -> In = read_input(),
           InA = atomize(In),
           {Hz, Depth} = journey2(InA, {0,0}, 0),
           Hz*Depth.