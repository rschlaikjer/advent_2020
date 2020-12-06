-module(a6).
-compile(export_all).

load_file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    binary:split(Bin, <<"\n">>, [global]).

process_groups(Lines) ->
    process_groups(Lines, sets:new(), []).
process_groups([], CG, Acc) -> [CG|Acc];
process_groups([L|Lines], CurGroup, Acc) ->
    case L of
        <<>> -> process_groups(Lines, sets:new(), [CurGroup|Acc]);
        _ ->
            % Push each character into the set
            NewGroup = sets:union(CurGroup, sets:from_list(binary_to_list(L))),
            process_groups(Lines, NewGroup, Acc)
    end.

sum_sets(Lines) ->
    QuestionSets = process_groups(Lines),
    Lengths = [sets:size(S) || S <- QuestionSets],
    lists:foldl(fun(A, B) -> A + B end, 0, Lengths).

full_set() ->
    sets:from_list(lists:seq($a, $z)).

process_p2(Lines) ->
    process_p2(Lines, full_set(), []).
process_p2([], CG, Acc) -> Acc;
process_p2([L|Lines], CurGroup, Acc) ->
    case L of
        <<>> -> process_p2(Lines, full_set(), [CurGroup|Acc]);
        _ ->
            % Push each character into the set
            NewGroup = sets:intersection(CurGroup, sets:from_list(binary_to_list(L))),
            process_p2(Lines, NewGroup, Acc)
    end.

sum_p2(Lines) ->
    QuestionSets = process_p2(Lines),
    Lengths = [sets:size(S) || S <- QuestionSets],
    lists:foldl(fun(A, B) -> A + B end, 0, Lengths).
