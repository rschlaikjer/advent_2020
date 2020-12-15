-module(a14).
-compile(export_all).

load_file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    [L || L <- binary:split(Bin, <<"\n">>, [global]), L =/= <<>>].

parse_line(<<"mask = ", L/binary>>) ->
    decode_mask(L);
parse_line(<<"mem[", L/binary>>) ->
    [AddrS, ValS] = binary:split(L, <<"] = ">>),
    {mem, binary_to_integer(AddrS), binary_to_integer(ValS)}.

decode_mask(Mask) ->
    {mask, A, O} = decode_mask(Mask, 0, 0),
    {mask, A, O, Mask}.
decode_mask(<<>>, And, Or) -> {mask, And, Or};
decode_mask(<<B, Rest/binary>>, And, Or) ->
    case B of
        $X -> decode_mask(Rest, (And bsl 1) + 1, Or bsl 1);
        $1 -> decode_mask(Rest, And bsl 1, (Or bsl 1) + 1);
        $0 -> decode_mask(Rest, And bsl 1, Or bsl 1)
    end.

-record(state, {memory, mask}).

execute(Lines) ->
    Instrs = [parse_line(L) || L <- Lines],
    State = #state{memory=#{}},
    lists:foldl(
        fun(Instr, Acc) ->
            case Instr of
                {mask, _A, _O} ->
                    Acc#state{mask=Instr};
                {mem, Addr, Val} ->
                    {mask, And, Or} = Acc#state.mask,
                    Mem1 = maps:put(Addr, (Val band And) bor Or, Acc#state.memory),
                    Acc#state{memory=Mem1}
            end
        end,
        State,
        Instrs
    ).

permute_address(Mask, Addr) when is_binary(Addr) ->
    permute_address(Mask, binary_to_list(Addr));
permute_address(_, []) -> [<<>>];
permute_address(<<M, Mask/binary>>, [B|Addrbits]) ->
    case byte_size(<<M, Mask/binary>>) > length([B|Addrbits]) of
        true -> permute_address(Mask, [B|Addrbits]);
        false ->
            SubAddresses = permute_address(Mask, Addrbits),
            case M of
                $0 ->
                    % Unchanged
                    [ <<B, S/binary>> || S <- SubAddresses];
                $1 ->
                    % Set high
                    [ <<1, S/binary>> || S <- SubAddresses];
                $X ->
                    % Floating
                    [ <<1, S/binary>> || S <- SubAddresses] ++
                    [ <<0, S/binary>> || S <- SubAddresses]
            end
    end.

addr_to_bin(Addr) ->
    addr_to_bin(Addr, []).
addr_to_bin(0, Acc) ->
    % If the acc is less than the 36 bits, pad it
    A1 = [0 || _ <- lists:seq(1, 36 - length(Acc))] ++ Acc,
    << <<X>> || X <- A1 >>;
addr_to_bin(Addr, Acc) ->
    case Addr band 1 of
        0 -> addr_to_bin(Addr bsr 1, [0 | Acc]);
        1 -> addr_to_bin(Addr bsr 1, [1 | Acc])
    end.

bin_to_addr(Bin) ->
    bin_to_addr(Bin, 0).
bin_to_addr(<<>>, Addr) -> Addr;
bin_to_addr(<<B, Rest/binary>>, Addr) ->
    bin_to_addr(Rest, (Addr bsl 1) + B).

execute2(Lines) ->
    Instrs = [parse_line(L) || L <- Lines],
    State = #state{memory=#{}},
    lists:foldl(
        fun(Instr, Acc) ->
            case Instr of
                {mask, _A, _O, _M} ->
                    Acc#state{mask=Instr};
                {mem, Addr, Val} ->
                    BinAddr = addr_to_bin(Addr),
                    {mask, _, _, MaskBin} = Acc#state.mask,
                    Permuted = permute_address(MaskBin, BinAddr),
                    PermAddrs = [bin_to_addr(A) || A <- Permuted],
                    Mem1 = lists:foldl(
                        fun(MAddr, MAcc) -> maps:put(MAddr, Val, MAcc) end,
                        Acc#state.memory,
                        PermAddrs
                    ),
                    Acc#state{memory=Mem1}
            end
        end,
        State,
        Instrs
    ).

memsum(#state{memory=M}) ->
    maps:fold(
        fun(_Addr, Val, Acc) ->
            Acc + Val
        end,
        0,
        M
    ).
