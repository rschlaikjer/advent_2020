-module(a15).
-compile(export_all).

-record(state, {last, hist}).

seed_state(Seed) ->
    seed_state(Seed, 1, #state{hist=#{}}).
seed_state([], _N, Acc) -> Acc;
seed_state([S|Seeds], N, Acc) ->
    PrevHist = maps:get(S, Acc#state.hist, []),
    State1 = #state{last = S, hist=maps:put(S, [N|PrevHist], Acc#state.hist)},
    seed_state(Seeds, N + 1, State1).

recite(Seed) when is_list(Seed) ->
    recite(length(Seed) + 1, seed_state(Seed)).

recite(30000001, State) -> State#state.last;
recite(N, State) ->
    Instances = maps:get(State#state.last, State#state.hist, []),
    NextN = case Instances of
        [_Single] ->
            % First time we have seen this number
            0;
        [R1|[R2|_]] ->
            % Repeat - get the two most recent times that number was said
            _Delta = R1 - R2
    end,
    NextInstances = maps:get(NextN, State#state.hist, []),
    State1 = #state{
        last=NextN,
        hist=maps:put(NextN, [N|NextInstances], State#state.hist)
    },
    % io:format("~p: ~p ~p~n", [N, NextN, State]),
    recite(N + 1, State1).
