-module(a9).
-compile(export_all).

load_file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    [ binary_to_integer(L) || L <- binary:split(Bin, <<"\n">>, [global]), L =/= <<>>].

pairfind(Preamble, Target) ->
    pairfind(Preamble, sets:from_list(Preamble), Target).
pairfind([], _Set, _Target) -> not_found;
pairfind([V|Values], Set, Target) ->
    % Does the complement to this number exist in the set?
    case sets:is_element(Target - V, Set) of
        true -> {ok, V, Target - V};
        false -> pairfind(Values, Set, Target)
    end.

find_outlier(Lines, PreambleLen) ->
    % Extract preamble
    {Preamble, Postamble} = lists:split(PreambleLen, Lines),
    find_outlier(Preamble, Postamble, PreambleLen).
find_outlier(Preamble, [N|Numbers], PreambleLen) ->
    % Update the preamble
    Preamble1 = tl(Preamble) ++ [N],
    % Is it possible to construct N using two of the numbers in the preamble?
    case pairfind(Preamble, N) of
        {ok, _, _} -> find_outlier(Preamble1, Numbers, PreambleLen);
        not_found -> {outlier, N}
    end.

sum_list(L) -> sum_list(L, 0).
sum_list([], Acc) -> Acc;
sum_list([N|Numbers], Acc) -> sum_list(Numbers, N + Acc).

find_cont_range(Lines, Target) ->
    find_cont_range(Lines, Target, []).
find_cont_range([L|Lines], Target, Subsection) ->
    % Take the sum of the current subsection
    SubsectionSum = sum_list(Subsection),
    case (SubsectionSum) of
        Target ->
            % This range sums correctly, return the first/last number
            {ok, Subsection};
        N1 when N1 < Target ->
            % Need to increase the size of the subsection
            Subsection1 = Subsection ++ [L],
            find_cont_range(Lines, Target, Subsection1);
        N2 when N2 > Target ->
            % Need to remove items from the subsection
            Subsection1 = tl(Subsection),
            find_cont_range([L|Lines], Target, Subsection1)
    end.

sum_minmax(Nums) ->
    sum_minmax(Nums, hd(Nums), hd(Nums)).
sum_minmax([], Min, Max) -> Min + Max;
sum_minmax([N|Numbers], Min, Max) ->
    sum_minmax(
      Numbers,
      case N < Min of true -> N; false -> Min end,
      case N > Max of true -> N; false -> Max end
    ).
