-module(a5).
-compile(export_all).

load_file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    [L || L <- binary:split(Bin, <<"\n">>, [global]), L =/= <<>>].

-record(partition, {xmin, xmax, ymin, ymax}).

default_range() ->
    #partition{
        xmin = 0,
        xmax = 127,
        ymin = 0,
        ymax = 7
    }.

seat_id(Range) ->
    trunc(Range#partition.xmax * 8 + Range#partition.ymax).

parse_partition(B) when is_binary(B) ->
    parse_partition(B, default_range()).
parse_partition(<<>>, Range) ->
    seat_id(Range);
parse_partition(B, Range) ->
    <<C:8, Rest/binary>> = B,
    XSpan = Range#partition.xmax - Range#partition.xmin,
    YSpan = Range#partition.ymax - Range#partition.ymin,
    XStep = math:ceil(XSpan / 2),
    YStep = math:ceil(YSpan / 2),
    NewPartition = case C of
        $F -> Range#partition{xmax = Range#partition.xmax - XStep};
        $B -> Range#partition{xmin = Range#partition.xmin + XStep};
        $L -> Range#partition{ymax = Range#partition.ymax - YStep};
        $R -> Range#partition{ymin = Range#partition.ymin + YStep}
    end,
    parse_partition(Rest, NewPartition).

highest_seat_id(Lines) ->
    Ids = [parse_partition(L) || L <- Lines],
    Max = lists:foldl(
        fun(A, B) -> case A > B of true -> A; false -> B end end,
        hd(Ids),
        tl(Ids)
    ).

missing_seats(Lines) ->
    Ids = [parse_partition(L) || L <- Lines],
    LowerOf = fun(A, B) -> case A < B of true -> A; false -> B end end,
    HigherOf = fun(A, B) -> case A > B of true -> A; false -> B end end,
    MinSeatId = lists:foldl(LowerOf, hd(Ids), tl(Ids)),
    MaxSeatId = lists:foldl(HigherOf, hd(Ids), tl(Ids)),
    AllSeats = sets:from_list(lists:seq(MinSeatId, MaxSeatId)),
    RemainingSeats = lists:foldl(
        fun sets:del_element/2,
        AllSeats,
        Ids
    ),
    lists:sort(sets:to_list(RemainingSeats)).
