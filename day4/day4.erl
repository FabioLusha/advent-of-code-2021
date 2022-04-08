-module(day4).
-define(FILE_NAME, "input.txt").
-import(lists, [seq/2]).
-import(linalg, [zeros/1, fill/2, zip_mat/2, unzip_mat/1]).
-compile([export_all, nowarn_export_all]).


start() ->
    io:format("Starto\nStarto\nStarto dayo!\n"),
    {WinStrike, Matrices} = format_input(read_input()),
    Sup = spawn(day4, create_sup, [WinStrike, Matrices]),
    Win = rpc(Sup, start),
    io:format("Winning value ~p\n", [Win]).

rpc(Pid, What) ->
    Pid ! {self() , What},
    receive
        {Pid, Response} -> Response
    end.

-spec read_input() -> bitstring() | none().
read_input() ->
    case file:read_file(?FILE_NAME) of
	{ok, Bin} -> Bin;
	{error, Why} -> throw(["Unable to open file\n", Why])
    end.

-spec winning_strike( bitstring() ) -> [integer()].
winning_strike(String) ->
    lists:map(
        fun(X) -> {Num, _} = string:to_integer(X), Num end,
        string:lexemes(String, ", ")
    ).

read_matrix(String) ->
    [lists:map(
        fun(X) -> {Num, _} = string:to_integer(X), Num end,
        string:lexemes(Row, " "))
    || Row <- string:lexemes(String,"\n")].

format_input(Bitstring) ->
    [A, B] = string:split(Bitstring, "\n\n", leading),
    WinStrike = winning_strike(A),
    Matrices = [read_matrix(Inp) || Inp <- string:split(B, "\n\n", all)],
    {WinStrike, Matrices}.

num_sender(Supervisor, [], _) ->
    Supervisor ! {self(), finished};
num_sender(Supervisor, [H | T], Pids) ->
    lists:foreach( fun(Pid) -> Pid ! {self(), H} end, Pids),
    receive
        {Supervisor, next} -> num_sender(Supervisor, T, Pids)
    end.

keep_trace(Supervisor, Mat = [H|_]) ->
    Nrows = length(Mat), Ncols = length(H),
    keep_trace(Supervisor, linalg:zeros(Nrows,Ncols) , Mat).

keep_trace(Supervisor, BitMap, Mat) ->
    receive
        {_, To_check} ->
            NewBitMap = check_elem(BitMap, Mat, To_check),
            case check_for_win(NewBitMap) of
                false ->
                    Supervisor ! {self(), no_win},
                    keep_trace(Supervisor, NewBitMap, Mat);
                true -> Supervisor ! {self(), {win, {NewBitMap, Mat}, To_check}}           
            end
    end.

check_elem(BitMap, Mat, Elem) -> 
    lists:map(
        fun(Row) ->
            lists:map(
                fun({MatEl, Bit}) ->
                    case MatEl == Elem andalso Bit == 0 of
                        true -> 1;
                        false -> Bit
                    end
                end,
                Row)
        end,
        linalg:zip_mat(Mat, BitMap)).

check_for_win(BitMap = [H|_]) ->
    % check if there is a row with all ones
    % if yes we won bingo
    case lists:member(length(BitMap), lists:map(fun sum/1, BitMap)) of
        true -> true;
        false ->
            % check if there is a completed column
            lists:member(length(H), lists:map(fun sum/1, linalg:get_cols(BitMap)))
    end.

sum(L) -> lists:foldl(fun(E, AccIn) -> AccIn+E end, 0, L).

member_at(Elem, L) -> member_at(Elem, L, 1).

member_at(_, [], _) -> {false, nil};
member_at(Elem, [H|T], N) ->
    case Elem == H of
        true -> {true, N};
        false -> member_at(Elem, T, N+1)
    end.

create_sup(WinStrike, Matrices) ->
    receive {Pid, start} ->
        Workers = lists:map(fun(X) -> spawn(day4, keep_trace, [self(), X]) end, Matrices),
        io:format("Workers ~p\n", [Workers]),
        Sender = spawn(day4, num_sender, [self(), WinStrike, Workers]),
        Pid ! {self(), wait_for_winner(Sender, 0, length(Workers))}
    end.

% we need to wait for replies to gurantee te correct first winner
wait_for_winner(Sender, N, N) ->
    io:format("Received ~p replies\n", [N]),
    Sender ! {self(), next},
    wait_for_winner(Sender, 0, N);
wait_for_winner(Sender, Workers_reply, Nworkers) ->
    receive
        {_, {win, {BitMap, Mat}, Value}} -> proclaim_winner(BitMap, Mat, Value);
        {_, no_win} -> wait_for_winner(Sender, Workers_reply+1, Nworkers)

    % we set a timer to wait for the replies
    % if unsure about the time wew can check if we receive all the replies with
    % the first clause of this function which is nevere matched if we don't
    % uncomment the second clause of the receive statment
%   after 0 ->
%      flush(),
%      Sender ! {self(), next},
%      wait_for_winner(Sender,Workers_reply,Nworkers)
    end.

proclaim_winner(BitMap, Mat, Value) ->
    io:format("Bitmap:\n~p\nMat:\n~p\n",[BitMap, Mat]),
    Sum = sum(
    lists:map(
        fun({_,Val}) -> Val end,
        lists:filter(
            fun({Bit, _}) -> Bit == 0 end,
            lists:flatten(
                linalg:zip_mat(BitMap,Mat)
            )
        )
    )),
    Sum * Value.

flush() ->
        receive
                _ -> flush()
        after
                0 -> ok
        end.

