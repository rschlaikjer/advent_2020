-module(a12).
-compile(export_all).

load_file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    [L || L <- binary:split(Bin, <<"\n">>, [global]), L =/= <<>>].

-record(instruction, {opcode, arg}).
-record(state, {x, y, heading}). % X = East/West, Y = North/South
-record(state2, {x, y, wx, wy}).

parse_instruction(L) ->
    <<Act:8, Arg/binary>> = L,
    #instruction{opcode=Act, arg=binary_to_integer(Arg)}.

wrap_angle(Angle) ->
    ((Angle rem 360) + 360) rem 360.

apply_instruction(State=#state{}, #instruction{opcode = $N, arg=Arg}) ->
    State#state{y = State#state.y + Arg};
apply_instruction(State=#state{}, #instruction{opcode = $S, arg=Arg}) ->
    State#state{y = State#state.y - Arg};
apply_instruction(State=#state{}, #instruction{opcode = $E, arg=Arg}) ->
    State#state{x= State#state.x + Arg};
apply_instruction(State=#state{}, #instruction{opcode = $W, arg=Arg}) ->
    State#state{x = State#state.x - Arg};
apply_instruction(State=#state{}, #instruction{opcode = $L, arg=Arg}) ->
    State#state{heading = State#state.heading - Arg};
apply_instruction(State=#state{}, #instruction{opcode = $R, arg=Arg}) ->
    State#state{heading = State#state.heading + Arg};
apply_instruction(State=#state{}, #instruction{opcode = $F, arg=Arg}) ->
    case wrap_angle(State#state.heading) of
        0 -> apply_instruction(State, #instruction{opcode = $E, arg=Arg});
        90 -> apply_instruction(State, #instruction{opcode = $S, arg=Arg});
        180 -> apply_instruction(State, #instruction{opcode = $W, arg=Arg});
        270 -> apply_instruction(State, #instruction{opcode = $N, arg=Arg})
    end.

displacement(Lines) ->
    Instructions = [parse_instruction(L) || L <- Lines],
    lists:foldl(
        fun(Instr, Acc) ->
            apply_instruction(Acc, Instr)
        end,
        #state{x=0, y=0, heading=0},
        Instructions
    ).

rotate_waypoint({X, Y}, Angle) ->
    case wrap_angle(Angle) of
        0 ->   {X, Y};
        90 ->  {Y, -X};
        180 -> {-X, -Y};
        270 -> {-Y, X}
    end.

apply_instruction2(State=#state2{}, #instruction{opcode = $N, arg=Arg}) ->
    State#state2{wy = State#state2.wy + Arg};
apply_instruction2(State=#state2{}, #instruction{opcode = $S, arg=Arg}) ->
    State#state2{wy = State#state2.wy - Arg};
apply_instruction2(State=#state2{}, #instruction{opcode = $E, arg=Arg}) ->
    State#state2{wx= State#state2.wx + Arg};
apply_instruction2(State=#state2{}, #instruction{opcode = $W, arg=Arg}) ->
    State#state2{wx = State#state2.wx - Arg};
apply_instruction2(State=#state2{}, #instruction{opcode = $L, arg=Arg}) ->
    {NX, NY} = rotate_waypoint({State#state2.wx, State#state2.wy}, -Arg),
    State#state2{wx = NX, wy = NY};
apply_instruction2(State=#state2{}, #instruction{opcode = $R, arg=Arg}) ->
    {NX, NY} = rotate_waypoint({State#state2.wx, State#state2.wy}, Arg),
    State#state2{wx = NX, wy = NY};
apply_instruction2(State=#state2{}, #instruction{opcode = $F, arg=Arg}) ->
    State#state2{x = State#state2.x + State#state2.wx * Arg,
                y = State#state2.y + State#state2.wy * Arg}.

displacement2(Lines) ->
    Instructions = [parse_instruction(L) || L <- Lines],
    lists:foldl(
        fun(Instr, Acc) ->
            apply_instruction2(Acc, Instr)
        end,
        #state2{x=0, y=0, wx=10, wy=1},
        Instructions
    ).


manhattan(#state2{x=X, y=Y}) -> abs(X) + abs(Y);
manhattan(#state{x=X, y=Y}) -> abs(X) + abs(Y).
