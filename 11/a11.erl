-module(a11).
-compile(export_all).

load_file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    [L || L <- binary:split(Bin, <<"\n">>, [global]), L =/= <<>>].

-record(seatmap, {rows, cols, data}).

seatmap_from_lines(Lines) ->
    Rows = length(Lines),
    Cols = byte_size(hd(Lines)),
    Data = [ [parse_char(C) || C <- binary_to_list(L)] || L <- Lines],
    #seatmap{rows=Rows, cols=Cols, data=Data}.

parse_char($.) -> floor;
parse_char($L) -> empty;
parse_char($#) -> occupied.

char_of_seat(floor) -> $.;
char_of_seat(empty) -> $L;
char_of_seat(occupied) -> $#.

% Rules:
% - Adjacency is 8 directions
% - If empty with no adjacencies, becomes occupied
% - If occupied and 4+ adjacencies are occupied, becomes empty

is_occupied(Seatmap, Row, Col) ->
    RowValid = Row >= 0 andalso Row < Seatmap#seatmap.rows,
    ColValid = Col >= 0 andalso Col < Seatmap#seatmap.cols,
    case RowValid andalso ColValid of
        false -> false; % Seat don't exist
        true ->
            SeatRow = lists:nth(Row+1, Seatmap#seatmap.data),
            SeatValue = lists:nth(Col+1, SeatRow),
            case SeatValue of
                floor -> false; % No sitting on floor
                empty -> false;
                occupied -> true
            end
    end.

adjacent_occupancy(Seatmap, Row, Col) ->
    Test = [
        {Row, Col + 1},
        {Row, Col - 1},
        {Row + 1, Col},
        {Row - 1, Col},
        {Row - 1, Col + 1},
        {Row + 1, Col + 1},
        {Row - 1, Col - 1},
        {Row + 1, Col - 1}
    ],
    lists:foldl(
        fun({R, C}, Acc) ->
            case is_occupied(Seatmap, R, C) of
                true -> Acc + 1;
                false -> Acc
            end
        end,
        0,
        Test
    ).

sightline_seat_occupied(Seatmap, Row, Col, StrideR, StrideC) ->
    RowValid = Row >= 0 andalso Row < Seatmap#seatmap.rows,
    ColValid = Col >= 0 andalso Col < Seatmap#seatmap.cols,
    case RowValid andalso ColValid of
        false -> false; % Seat don't exist
        true ->
            SeatRow = lists:nth(Row+1, Seatmap#seatmap.data),
            SeatValue = lists:nth(Col+1, SeatRow),
            case SeatValue of
                floor ->
                    sightline_seat_occupied(
                      Seatmap, Row + StrideR, Col + StrideC, StrideR, StrideC);
                empty -> false;
                occupied -> true
            end
    end.

sightline_occupancy(Seatmap, Row, Col) ->
    Strides = [
        {0, +1}, {0, -1}, {+1, 0}, {-1, 0},
        {-1, +1}, {+1, +1}, {-1, -1}, {+1, -1}
    ],
    lists:foldl(
        fun({RS, CS}, Acc) ->
            case sightline_seat_occupied(Seatmap, Row+RS, Col+CS, RS, CS) of
                true -> Acc + 1;
                false -> Acc
            end
        end,
        0,
        Strides
    ).

iterate_seatmap(Seatmap=#seatmap{}, OccupancyFun) ->
    iterate_seatmap(Seatmap, OccupancyFun, Seatmap#seatmap.data, 0, Seatmap#seatmap{data=[]}).
iterate_seatmap(_Seatmap, _OccupancyFun, [], _RowC, Acc) -> Acc;
iterate_seatmap(Seatmap, OccupancyFun, [Row|Rows], RowC, Acc) ->
    % Generate the new data row
    {_, Row1} = lists:foldl(
        fun(Seat, {N, Seats}) ->
            AdjOcc = OccupancyFun(Seatmap, RowC, N),
            NewSeat = case {Seat, AdjOcc} of
                {empty, 0} -> occupied;
                {occupied, Adj} when Adj >= 5 -> empty;
                _ -> Seat
            end,
            {N+1, [NewSeat|Seats]}
        end,
        {0, []},
        Row
    ),
    Acc1 = Acc#seatmap{data=Acc#seatmap.data ++ [lists:reverse(Row1)]},
    iterate_seatmap(Seatmap, OccupancyFun, Rows, RowC+1, Acc1).

iteraten_seatmap(0, Seatmap, _OccFun) -> Seatmap;
iteraten_seatmap(N, Seatmap, OccFun) ->
    iteraten_seatmap(N - 1, iterate_seatmap(Seatmap, OccFun), OccFun).

iterate_until_stable(Seatmap, OccFun) ->
    iterate_until_stable(Seatmap, OccFun, 1, iterate_seatmap(Seatmap, OccFun)).
iterate_until_stable(Seatmap, OccFun, N, Acc) ->
    case Seatmap =:= Acc of
        true -> Seatmap;
        false ->
            iterate_until_stable(Acc, OccFun, N + 1, iterate_seatmap(Acc, OccFun))
    end.

print_smap(#seatmap{data=Data}) ->
    lists:foreach(
        fun(Row) ->
            Line = [char_of_seat(S) || S <- Row],
            io:format("~s~n", [Line])
        end,
        Data
    ).

occupied_count(#seatmap{data=Data}) ->
    RowCount = fun(Row) ->
        lists:foldl(
            fun(Seat, Acc) ->
                case Seat of
                    occupied -> Acc + 1;
                    _ -> Acc
                end
            end,
            0,
            Row
        )
    end,
    lists:foldl(
        fun(Row, Acc) ->
            Acc + RowCount(Row)
        end,
        0,
        Data
    ).
