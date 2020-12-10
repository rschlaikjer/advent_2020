-module(a10).
-compile(export_all).

load_file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    [ binary_to_integer(L) || L <- binary:split(Bin, <<"\n">>, [global]), L =/= <<>>].

-record(state, {one_jolt_count=0, three_jolt_count=1}).

chain_jolts(Lines) ->
    chain_jolts(0, sets:from_list(Lines), #state{}).
chain_jolts(CurrentJolts, Adapters, State=#state{}) ->
    case sets:size(Adapters) of
        0 -> State;
        _ ->
            % Do we have an adapter that is same/one/two/three jolts away from
            % the current joltage?
            TestJolts = lists:seq(CurrentJolts, CurrentJolts + 3),
            ValidJolts = [J || J <- TestJolts, sets:is_element(J, Adapters)],
            NextAdapter = lists:min(ValidJolts),
            Adapters1 = sets:del_element(NextAdapter, Adapters),
            State1 = case NextAdapter - CurrentJolts of
                1 -> State#state{one_jolt_count = State#state.one_jolt_count + 1};
                3 -> State#state{three_jolt_count = State#state.three_jolt_count + 1};
                _ -> State
            end,
            chain_jolts(NextAdapter, Adapters1, State1)
    end.

tree_jolts(Lines) ->
    tree_jolts(0, sets:from_list(Lines), #{}).
tree_jolts(CurrentJolts, Adapters, Memo) ->
    % Clear out all adapters < current adapter from set
    MapKey = {CurrentJolts, sets:fold(
        fun(K, Acc) ->
            case K > CurrentJolts of
                true -> sets:add_element(K, Acc);
                false -> Acc
            end
        end,
        sets:new(),
        Adapters
    )},

    % Check sub-paths using our memo
    case maps:get(MapKey, Memo, not_found) of
        not_found ->
            TestJolts = lists:seq(CurrentJolts, CurrentJolts + 3),
            ValidJolts = [J || J <- TestJolts, sets:is_element(J, Adapters)],
            case length(ValidJolts) of
                0 -> {1, Memo};
                _ ->
                    % Count the results for each possible forward choice
                    {Paths, Memo1} = lists:foldl(
                        fun(NextAdapter, {AccPaths, AccMemo}) ->
                            Adapters1 = sets:del_element(NextAdapter, Adapters),
                            {SubPaths, SubMemo} = tree_jolts(NextAdapter, Adapters1, AccMemo),
                            {SubPaths + AccPaths, SubMemo}
                        end,
                        {0, Memo},
                        ValidJolts
                    ),
                    {Paths, maps:put(MapKey, Paths, Memo1)}
            end;
        N when is_integer(N) ->
            {N, Memo}
    end.
