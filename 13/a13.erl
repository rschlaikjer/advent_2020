-module(a13).
-compile(export_all).

load_file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    [L || L <- binary:split(Bin, <<"\n">>, [global]), L =/= <<>>].

parse_buses(Line) ->
    Buses = binary:split(Line, <<",">>, [global]),
    [ case B of <<"x">> -> no_service; _ -> binary_to_integer(B) end || B <- Buses].

firstbus(Lines) ->
    Ts = binary_to_integer(hd(Lines)),
    Buses = parse_buses(hd(tl(Lines))),
    lists:foldl(
        fun(Bus, Acc) ->
            {AccTime, _AccBus} = Acc,
            case Bus of
                no_service -> Acc;
                _ ->
                    WaitTime = case Ts rem Bus of
                        0 -> 0;
                        N -> Bus - N
                    end,
                    io:format("~p: ~p~n", [Bus, WaitTime]),
                    case WaitTime < AccTime of
                        true -> {WaitTime, Bus};
                        false -> Acc
                    end
            end
        end,
        {undefined, undefined},
        Buses
    ).

valid_departures(Ts, Offsets) ->
    valid_departures(Ts, 0, Offsets).
valid_departures(_Ts, _Step, []) -> true;
valid_departures(Ts, Step, [{Offset, Bus}|Offsets]) ->
    case (Ts + Offset) rem Bus of
        0 -> valid_departures(Ts, Step + Bus, Offsets);
        N -> {false, (Bus - N)}
    end.

bus_offsets(Buses) ->
    bus_offsets(Buses, 0, []).
bus_offsets([], _BusCount, Acc) ->
    % Sort such that larges bus id is first
    lists:sort(
        fun(O1, O2) ->
            {_, B1} = O1,
            {_, B2} = O2,
            B2 < B1
        end,
        Acc
    );
bus_offsets([Bus|Buses], BusCount, Acc) ->
    case Bus of
        no_service ->
            bus_offsets(Buses, BusCount + 1, Acc);
        _ ->
            bus_offsets(Buses, BusCount + 1, [{BusCount, Bus}|Acc])
    end.

earliest_bus_bf(Lines) ->
    Buses = parse_buses(hd(tl(Lines))),
    Offsets = bus_offsets(Buses),
    Test = fun(Self, Ts) ->
        case valid_departures(Ts, Offsets) of
            true -> Ts;
            false -> Self(Self, Ts)
        end
    end,
    Test(Test, 0).

earliest_bus(Offsets) ->
    earliest_bus(0, Offsets).
earliest_bus(Ts, Offsets) ->
    case Ts rem 50000000 of
        0 -> io:format("~p~n", [Ts]);
        _ -> ok
    end,
    case valid_departures(Ts, Offsets) of
        true -> {ok, Ts};
        {false, Step} ->
            earliest_bus(Ts + Step, Offsets)
    end.

mul_inv(A, B) ->
    mul_inv(B, 0, 1, A, B).
mul_inv(B0, X0, X1, A, 1) -> 1;
mul_inv(B0, X0, X1, A, B) ->
    case A > 1 of
        false ->
            case X1 < 0 of true -> X1 + B0; false -> X1 end;
        true ->
            Q = A div B,
            mul_inv(B0, X1 - Q * X0, X0, B, A rem B)
    end.

egcd(_, 0) -> {1, 0};
egcd(A, B) ->
    {S, T} = egcd(B, A rem B),
    {T, S - (A div B)*T}.

mod_inv(A, B) ->
    {X, Y} = egcd(A, B),
    if
        A*X + B*Y =:= 1 -> X;
        true -> undefined
    end.

mod(A, M) ->
    case A rem M of
        X when X < 0 -> X + M;
        V -> V
    end.

calc_inverses([], []) -> [];
calc_inverses([N | Ns], [M | Ms]) ->
    case mod_inv(N, M) of
        undefined -> undefined;
        Inv -> [Inv | calc_inverses(Ns, Ms)]
    end.

crem(Congruences) ->
    {Residues, Modulii} = lists:unzip(Congruences),
    ModPI = lists:foldl(fun(A, B) -> A*B end, 1, Modulii),
    CRT_Modulii = [ModPI div M || M <- Modulii],
    case calc_inverses(CRT_Modulii, Modulii) of
        undefined -> undefined;
        Inverses ->
            Solution = lists:sum([A*B || {A,B} <- lists:zip(CRT_Modulii,
                                    [A*B || {A,B} <- lists:zip(Residues, Inverses)])]),
            mod(Solution, ModPI)
    end.

crt_bus(Lines) ->
    Buses = a13:parse_buses(hd(tl(Lines))),
    Offsets = a13:bus_offsets(Buses),
    Congruences = [{Bus - Off rem Bus, Bus} || {Off, Bus} <- Offsets],
    crem(Congruences).
