-module(linalg).
-export([
    fill/2,
    fill/3,
    zeros/1,
    zeros/2,
    ones/1,
    ones/2,
    identity/1,
    get/3,
    insert/3,
    insert/4,
    get_row/2,
    get_col/2,
    get_cols/1,
    vec_contains/2,
    mat_contains/2,
    add_vec/2,
    add_mat/2,
    zip_mat/2,
    unzip_mat/1
]).

fill(N, Dim) ->
    [N || _ <- lists:seq(1, Dim)].

fill(N, Rows, Cols) ->
    [fill(N, Cols) || _ <- lists:seq(1, Rows)].

zeros(N) -> fill(0, N).
zeros(Rows, Cols) -> fill(0, Rows, Cols).

ones(N) -> fill(1, N).
ones(Rows, Cols) -> fill(1, Rows, Cols).

identity(N) -> identity(N,0,[]).

identity(N,N, Acc) -> lists:reverse(Acc);
identity(N,H, Acc) ->
    identity(N, H+1, [insert(zeros(N), H+1, 1)|Acc]).

dim(Mat) -> {length(Mat), length(hd(Mat))}.

get(Mat, Row, Col) ->
    lists:nth(Col, lists:nth(Row, Mat)).

get_row(Mat, Row) ->
    lists:nth(Row, Mat).

get_col(Mat, Col) ->
    [lists:nth(Col, Row) || Row <- Mat].

get_cols(Mat = [H|_]) ->
    Ncols = length(H),
    [get_col(Mat, Ncol) || Ncol <- lists:seq(1,Ncols)].

vec_contains(Vec, Elem) ->
    vec_contains(Vec, Elem, 1, []).

vec_contains([],_,_,Acc) -> lists:reverse(Acc);
vec_contains([H|T], Elem, Pos, Acc) ->
    case H == Elem of
        true -> vec_contains(T, Elem, Pos+1, [Pos|Acc]);
        false -> vec_contains(T,Elem,Pos+1, Acc)
    end.

mat_contains(Mat, Elem) -> 
    {_, Posts} = lists:foldl(
               fun(Row, _AccIn = {Pos, Finds}) ->
                   case vec_contains(Row, Elem) of
                       [] -> {Pos+1, Finds};
                       L -> {Pos+1, lists:map(fun(X) -> {Pos, X} end, L) ++ Finds}
                   end
               end,
               {1,[]},
               Mat),
    lists:reverse(Posts).

insert(Vec,Pos, Elem) ->   
    [(fun
         ({_Col,P}) when _Col /= Pos -> P;
         (_) -> Elem
     end)({Col, X})
    || {Col, X} <- lists:zip(lists:seq(1,length(Vec)), Vec)].

insert(Mat, RowInd, ColInd, Elem) ->
    [(fun
         ({IndR, _Row}) when IndR /= RowInd -> _Row;
         ({_, _Row}) -> insert(Row, ColInd, Elem)
     end)({Index, Row})
    || {Index, Row} <- lists:zip(lists:seq(1,length(Mat)), Mat)].

zip_mat(A,B) ->
    [lists:zip(Ea,Eb) || {Ea, Eb} <- lists:zip(A,B)].

unzip_mat(Mat) ->
    {UzipA, UzipB} = lists:foldl(
                        fun(Elem, _AccIn = {A,B}) ->
                            {Ae, Be} = lists:unzip(Elem),
                            {[Ae|A], [Be|B]}
                        end,
                        {[],[]},
                        Mat),
    {lists:reverse(UzipA), lists:reverse(UzipB)}.

add_vec(Vec1, Vec2) ->
    lists:map(fun({X1,X2}) -> X1+X2 end, lists:zip(Vec1,Vec2)).

add_mat(M1, M2) ->
    case dim(M1) == dim(M2) of
        true ->
            lists:map(fun({V1,V2}) -> add_vec(V1,V2) end, lists:zip(M1,M2));
        false -> throw(["Case not handled"])
    end.