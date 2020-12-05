-module(a2).
-compile(export_all).

-record(pwd, {min, max, char, text}).

ccount(B, C) when is_binary(B) ->
    ccount(B, C, 0).
ccount(<<>>, _MC, Acc) -> Acc;
ccount(<<C:8, B/binary>>, MatchChar, Acc) ->
    case C =:= MatchChar of
        true -> ccount(B, MatchChar, Acc+1);
        false -> ccount(B, MatchChar, Acc)
    end.

validate_p1(P = #pwd{}) ->
    Count = ccount(P#pwd.text, P#pwd.char),
    Count >= P#pwd.min andalso Count =< P#pwd.max.

validate_p2(P = #pwd{}) ->
    Chars = binary_to_list(P#pwd.text),
    Char1 = lists:nth(P#pwd.min, Chars),
    Char2 = lists:nth(P#pwd.max, Chars),
    Match1 = case Char1 =:= P#pwd.char of true -> 1; false -> 0 end,
    Match2 = case Char2 =:= P#pwd.char of true -> 1; false -> 0 end,
    Valid = Match1 bxor Match2,
    case Valid of 1 -> true; 0 -> false end.

cound_valid(Passwords, Validator) ->
    lists:foldl(
        fun(P, Acc) ->
            Acc + case Validator(P) of true -> 1; false -> 0 end
        end,
        0,
        Passwords
    ).

parse_file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    Lines = [L || L <- binary:split(Bin, <<"\n">>, [global]), L =/= <<>>],
    lists:foldl(
        fun(Line, Acc) ->
            [Pair, R1] = binary:split(Line, <<" ">>),
            [MinS, MaxS] = binary:split(Pair, <<"-">>),
            <<Char:8, ": ", Password/binary>> = R1,
            R = #pwd{
               min = binary_to_integer(MinS),
               max = binary_to_integer(MaxS),
               char = Char,
               text = Password
            },
            [R|Acc]
        end,
        [],
        Lines
    ).
