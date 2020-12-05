-module(a1).
-compile(export_all).

pairfind([], _Set, _Target) -> not_found;
pairfind([V|Values], Set, Target) ->
    % Does the complement to this number exist in the set?
    case sets:is_element(Target - V, Set) of
        true -> V * (Target - V);
        false -> pairfind(Values, Set, Target)
    end.

tripfind(List, Set, Target) ->
    tripfind(List, List, Set, Target).
tripfind([], _List, _Set, _Target) -> not_found;
tripfind([V|Values], List, Set, Target) ->
    case pairfind(List, Set, 2020 - V) of
        not_found -> tripfind(Values, List, Set, Target);
        I when is_integer(I) ->
            V * I
    end.

find_sum(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    Lines = binary:split(Bin, <<"\n">>, [global]),
    NumList = [binary_to_integer(B) || B <- Lines, B =/= <<>>],
    NumSet = sets:from_list(NumList),
    tripfind(NumList, NumSet, 2020).
