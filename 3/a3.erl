-module(a3).
-compile(export_all).

-record(traj, {x, y}).

load_file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    _Lines = [L || L <- binary:split(Bin, <<"\n">>, [global]), L =/= <<>>].

is_tree($.) -> false;
is_tree($#) -> true.

count_trees(T=#traj{}, Lines) ->
    count_trees(T, Lines, 0, 0).
count_trees(T=#traj{x=Tx, y=Ty}, [L|Lines], XIdx, Acc) when length([L|Lines]) >= Ty ->
    io:format("Active line: ~p~n", [L]),
    % Get the square at this index
    Obstacle = lists:nth(1 + XIdx, binary_to_list(L)),
    io:format("Idx: ~p, Char: ~c~n", [XIdx, Obstacle]),
    % Count across to the tree index (with wrapping)
    NX = (XIdx + Tx) rem byte_size(L),
    % Increment tree count?
    TreeCount = Acc + case is_tree(Obstacle) of true -> 1; false -> 0 end,
    % Skip lines until we get to the next line in the trajectory
    {SkipLines, TrajLines} = lists:split(Ty, [L|Lines]),
    io:format("Skipped lines: ~p~n", [SkipLines]),
    % Continue
    count_trees(T, TrajLines, NX, TreeCount);
count_trees(#traj{}, Lines, _XIdx, Acc) ->
    io:format("Remaining lines: ~p~n", [Lines]),
    Acc.

test_trajectories() -> [
    #traj{x = 1, y = 1},
    #traj{x = 3, y = 1},
    #traj{x = 5, y = 1},
    #traj{x = 7, y = 1},
    #traj{x = 1, y = 2}
].

mult_trajectories(Trajectories, Lines) ->
    Trees = [count_trees(T, Lines) || T <- Trajectories],
    [io:format("Tree counts: ~p~n", [T]) || T <- Trees],
    lists:foldl(fun(A, B) -> A * B end, 1, Trees).
