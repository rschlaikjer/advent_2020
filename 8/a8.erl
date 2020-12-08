-module(a8).
-compile(export_all).

load_file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    binary:split(Bin, <<"\n">>, [global]).

parse_instruction(<<"nop">>) -> nop;
parse_instruction(<<"acc">>) -> acc;
parse_instruction(<<"jmp">>) -> jmp.

parse_lines(Lines) ->
    parse_lines(Lines, []).
parse_lines([], Acc) -> lists:reverse(Acc);
parse_lines([<<>>|Lines], Acc) -> parse_lines(Lines, Acc);
parse_lines([L|Lines], Acc) ->
    [Instr, Arg] = binary:split(L, <<" ">>),
    parse_lines(Lines, [{parse_instruction(Instr), binary_to_integer(Arg)}|Acc]).

-record(program, {acc = 0, pc = 0}).

execute_program(Instructions) ->
    execute_program(Instructions, #program{}, sets:new()).
execute_program(Instructions, Prog=#program{}, ExecutedPCs) ->
    PC = Prog#program.pc,
    Acc = Prog#program.acc,
    case PC < length(Instructions) of
        false ->
            {program_complete, Prog};
        true ->
            {Opcode, Arg} = lists:nth(PC + 1, Instructions),
            Prog1 = case Opcode of
                nop -> Prog#program{pc = PC + 1};
                acc -> Prog#program{acc = Acc + Arg, pc = PC + 1};
                jmp -> Prog#program{pc = PC + Arg}
            end,
            io:format("Execute: ~p, ~p -> ~p~n", [{Opcode, Arg}, Prog, Prog1]),
            ExecutePCs1 = sets:add_element(PC, ExecutedPCs),
            case sets:is_element(PC, ExecutedPCs) of
                true ->
                    % Infinite loop detected
                    {infinite_loop, Prog};
                false ->
                    execute_program(Instructions, Prog1, ExecutePCs1)
            end
    end.

% Try swapping each nop to jmp and vice versa to try and make the program exit
bforce_program(Instructions) ->
    bforce_program([], Instructions).
bforce_program(Seen, [Instr|Instrs]) ->
    {Opcode, Arg} = Instr,
    NewProgram = case Opcode of
        nop ->
            % Mutate to jmp
            Seen ++ [{jmp, Arg}|Instrs];
        jmp ->
            % Mutate to nop
            Seen ++ [{nop, Arg}|Instrs];
        acc -> Seen ++ [Instr|Instrs]
    end,
    case execute_program(NewProgram) of
        {program_complete, Prog} ->
            Prog;
        {infinite_loop, _} ->
            bforce_program(Seen ++ [Instr], Instrs)
    end.
