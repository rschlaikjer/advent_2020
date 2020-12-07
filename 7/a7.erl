-module(a7).
-compile(export_all).

load_file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    binary:split(Bin, <<"\n">>, [global]).

-record(held, {colour, count}).
-record(rule, {container, holds}).

binary_join([B|Bs]) ->
    binary_join(Bs, B).
binary_join([], Acc) -> Acc;
binary_join([B|Bs], Acc) ->
    binary_join(Bs, <<Acc/binary, " ", B/binary>>).

parse_hold(Hold) ->
    % Split on first " " for count
    Fragments = binary:split(Hold, <<" ">>, [global]),
    Count = binary_to_integer(hd(Fragments)),
    % Drop the "bag[s]" from the end
    ColourL = lists:reverse(tl(lists:reverse(tl(Fragments)))),
    Colour = binary_join(ColourL),
    #held{colour=Colour, count=Count}.

parse_rules(Lines) ->
    parse_rules(Lines, []).
parse_rules([], Acc) -> Acc;
parse_rules([<<>>|Lines], Acc) -> parse_rules(Lines, Acc);
parse_rules([Line|Lines], Acc) ->
    [Container, Contains] = binary:split(Line, <<" bags contain ">>),
    Rule = case Contains of
        <<"no other bags.">> -> #rule{container=Container, holds=[]};
        _ ->
            % Get rid of the trailing .
            Dotless = binary:replace(Contains, <<".">>, <<>>),
            % Split
            Holds = binary:split(Dotless, <<", ">>, [global]),
            #rule{container=Container, holds=[parse_hold(H) || H <- Holds]}
    end,
    parse_rules(Lines, [Rule|Acc]).

invert_index(Rules) ->
    invert_index(Rules, ets:new(inv_idx, [duplicate_bag])).
invert_index([], Acc) -> Acc;
invert_index([R|Rules], Acc) ->
    % For each contained item, add a mapping of contained -> container
    lists:foreach(
        fun(#held{colour=HeldColour}) ->
            ets:insert(Acc, {HeldColour, R#rule.container})
        end,
        R#rule.holds
    ),
    invert_index(Rules, Acc).

holders_of_colour(Ets, Colour) ->
    [V || {_K, V} <- ets:lookup(Ets, Colour)].

recursive_holders(Ets, Colour) ->
    recursive_holders(Ets, [Colour], sets:new()).
recursive_holders(_Ets, [], Valid) -> sets:to_list(Valid);
recursive_holders(Ets, [Colour|Search], Valid) ->
    % Get all the holders of the next colour in the list
    Holders = holders_of_colour(Ets, Colour),
    % Add all to the valid set
    Valid1 = sets:union(Valid, sets:from_list(Holders)),
    % For all those _not_ already in the valid set, add them to the search list
    Search1 = Search ++ lists:foldl(
        fun(C, Acc) ->
            case sets:is_element(C, Valid) of
                true -> Acc;
                false -> [C|Acc]
            end
        end,
        [],
        Holders
    ),
    recursive_holders(Ets, Search1, Valid1).

solve_p1(File) ->
    Lines = load_file(File),
    Rules = parse_rules(Lines),
    Ets = invert_index(Rules),
    length(recursive_holders(Ets, <<"shiny gold">>)).

rule_for_colour(_C, []) -> not_found;
rule_for_colour(C, [R|Rules]) ->
    case R#rule.container of
        C -> R;
        _ -> rule_for_colour(C, Rules)
    end.

held_count(Colour, Rules) ->
    % Get the rule for this colour
    Rule = rule_for_colour(Colour, Rules),
    % Sum the child bag count
    lists:foldl(
        fun(#held{colour=Col, count=Cnt}, Acc) ->
                Acc + Cnt + Cnt * held_count(Col, Rules)
        end,
        0,
        Rule#rule.holds
    ).
