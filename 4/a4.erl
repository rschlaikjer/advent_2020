-module(a4).
-compile(export_all).

-record(passport, {byr, iyr, eyr, hgt, hcl, ecl, pid, cid}).

load_file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    binary:split(Bin, <<"\n">>, [global]).

parse_tuple(T) ->
    [K, V] = binary:split(T, <<":">>),
    {K, V}.

line_to_pairs(Line) ->
    StringTuples = binary:split(Line, <<" ">>, [global]),
    [parse_tuple(T) || T <- StringTuples].

update_passport([], Passport) -> Passport;
update_passport([T|Tuples], Passport) ->
    {K, V} = T,
    NewPassport = case K of
        <<"byr">> -> Passport#passport{byr=V};
        <<"iyr">> -> Passport#passport{iyr=V};
        <<"eyr">> -> Passport#passport{eyr=V};
        <<"hgt">> -> Passport#passport{hgt=V};
        <<"hcl">> -> Passport#passport{hcl=V};
        <<"ecl">> -> Passport#passport{ecl=V};
        <<"pid">> -> Passport#passport{pid=V};
        <<"cid">> -> Passport#passport{cid=V};
        _ -> Passport
    end,
    update_passport(Tuples, NewPassport).

passport_empty(P=#passport{}) ->
    P#passport.byr =:= undefined andalso
    P#passport.iyr =:= undefined andalso
    P#passport.eyr =:= undefined andalso
    P#passport.hgt =:= undefined andalso
    P#passport.hcl =:= undefined andalso
    P#passport.ecl =:= undefined andalso
    P#passport.pid =:= undefined andalso
    P#passport.cid =:= undefined.

parse_passports(Lines) ->
    parse_passports(Lines, #passport{}, []).
parse_passports([], Current=#passport{}, Acc) ->
    [P || P <- [Current|Acc], not passport_empty(P)];
parse_passports([L|Lines], Current=#passport{}, Acc) ->
    case L of
        <<>> ->
            % Empty line - conclude the active passport record & start
            % a new one.
            parse_passports(Lines, #passport{}, [Current|Acc]);
        _ ->
            % Take the current line and split into KV pairs
            Tuples = line_to_pairs(L),
            % For the possible field values, if any are present in the pair
            % list, update the current passport record
            NewCurrent = update_passport(Tuples, Current),
            parse_passports(Lines, NewCurrent, Acc)
    end.

passport_valid_p1(P=#passport{}) ->
    % All fields are non-optional except CID
    P#passport.byr =/= undefined andalso
    P#passport.iyr =/= undefined andalso
    P#passport.eyr =/= undefined andalso
    P#passport.hgt =/= undefined andalso
    P#passport.hcl =/= undefined andalso
    P#passport.ecl =/= undefined andalso
    P#passport.pid =/= undefined.

passport_valid_p2(P=#passport{}) ->
    % Passports that fail initial validation are immediately invalid
    P1Valid = passport_valid_p1(P),

    % 1920 <= BYR <= 2002
    ValidateBYR = fun(Byr) ->
        ByrInt = binary_to_integer(Byr),
        1920 =< ByrInt andalso ByrInt =< 2002
    end,

    % 2010 <= IYR <= 2020
    ValidateIYR = fun(Iyr) ->
        IyrInt = binary_to_integer(Iyr),
        2010 =< IyrInt andalso IyrInt =< 2020
    end,

    % 2020 <= EYR <= 2030
    ValidateEYR = fun(Eyr) ->
        EyrInt = binary_to_integer(Eyr),
        2020 =< EyrInt andalso EyrInt =< 2030
    end,

    % HGT: number + in/cm
    %   150 <= cm <= 193
    %   59 <= in <= 76
    IsInt = fun(C) -> $0 =< C andalso C =< $9 end,
    ValidateHGT = fun(Hgt) ->
        % Split into int part and cm/in part
        HgtList = binary_to_list(Hgt),
        IntS = lists:takewhile(IsInt, HgtList),
        Unit = lists:dropwhile(IsInt, HgtList),
        IntVal = list_to_integer(IntS),
        case Unit of
            "in" -> 59 =< IntVal andalso IntVal =< 76;
            "cm" -> 150 =< IntVal andalso IntVal =< 193;
            _ -> false
        end
    end,

    % HCL: Hex colour code
    IsHex = fun(C) ->
        IsInt(C) orelse
        ($a =< C andalso C =< $f) orelse
        ($A =< C andalso C =< $F)
    end,
    ValidateHCL = fun(Hcl) ->
        HclList = binary_to_list(Hcl),
        hd(HclList) =:= $# andalso lists:all(IsHex, tl(HclList))
    end,

    % ECL: Subset {amb, blu, brn, gry, grn, hzl, oth}
    ValidEcls = [<<"amb">>, <<"blu">>, <<"brn">>, <<"gry">>, <<"grn">>, <<"hzl">>, <<"oth">>],
    ValidateECL = fun(Ecl) ->
        lists:any(fun(E) -> Ecl =:= E end, ValidEcls)
    end,

    % PID: 9 digit number
    ValidatePID = fun(Pid) ->
        lists:all(IsInt, binary_to_list(Pid)) andalso
        byte_size(Pid) =:= 9
    end,

    % Chain it all together
    P1Valid andalso
    ValidateBYR(P#passport.byr) andalso
    ValidateIYR(P#passport.iyr) andalso
    ValidateEYR(P#passport.eyr) andalso
    ValidateHGT(P#passport.hgt) andalso
    ValidateHCL(P#passport.hcl) andalso
    ValidateECL(P#passport.ecl) andalso
    ValidatePID(P#passport.pid).

count_valid(Passports, Validator) ->
    lists:foldl(
        fun(P, Acc) ->
            case Validator(P) of
                true -> Acc + 1;
                false -> Acc
            end
        end,
        0,
        Passports
    ).
