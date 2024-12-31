:- module day7.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.
:- implementation.
:- import_module list, string, int, char, bool, solutions, exception.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                %%%
%%%                             solver                             %%%
%%%                                                                %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- type operator ---> plus ; mult ; conc.

:- pred operator_result_int_int(operator, int, int, int).
:- mode operator_result_int_int(out, in, in, in) is nondet.
:- mode operator_result_int_int(out, out, in, in) is multi.
:- mode operator_result_int_int(in, out, in, in) is det.
operator_result_int_int(plus, X + Y, X, Y).
operator_result_int_int(mult, X * Y, X, Y).
operator_result_int_int(conc, det_to_int(int_to_string(X) ++ int_to_string(Y)), X, Y).
% ^ COMPILER UB?
%   if I compile without optimizations, this fail and makes all the lines fail!
%   but if I put a call to this last predicate in main it works! (see comment in main)
%   or if a do not put a call to this predicate, but compile with: mmc -OX ... X!=2, it works!
%   the problem seems to be in -O2 !?!?
% edit: what works are the tests, running with input yields segment violation
%   related: https://github.com/Mercury-Language/mercury/issues/103
%            https://bugs.gentoo.org/846974
%            https://bugs.mercurylang.org/view.php?id=561
%   a compiler compilation flag ... maybe?

% :- type equation_numbers == {int, list.list(int)}.
%   ^ this works until you try to define a typeclass ...
%     typeclasses are fairly strict, they only take a functor -and
%     only one, no nesting allowed :'(- and type variables
% but in this case the problem is in the values, If I do not make this I have no contructors
% this is similar of newtype/data in haskell!
:- type equation_numbers ---> equats(int, list.list(int)).

:- type solved_equation
    ---> single_val(int)
    ;    result_tree({int, {solved_equation, operator, solved_equation}}).

:- func value(solved_equation) = int is det.
value(single_val(X)) = X.
value(result_tree({X, _})) = X.

:- pred solve(equation_numbers::in, solved_equation::out) is nondet.
solve(equats(R, [X|Numbers]), Solution) :- foldl(
    (pred(Number::in, Current::in, Next::out) is nondet
        :- N = value(Current),
           operator_result_int_int(Op, M, N, Number),
           M =< R,
           Next = result_tree(
               {M, {Current, Op, single_val(Number)}}
           )
    ),
    Numbers, single_val(X), Solution),
    Solution = result_tree({R, _}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                %%%
%%%                            printing                            %%%
%%%                                                                %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- typeclass human_readable(T) where [
    func to_string(T) = string is det
].

:- instance human_readable(equation_numbers) where [
    to_string(equats(X, Lst)) = string.format("%i =?= %s", [i(X), s(string(Lst))])
].

:- instance human_readable(operator) where [
    to_string(plus) = "+",
    to_string(mult) = "x",
    to_string(conc) = "||"
].

% polemic ABUSE ...  it would be better to just do several functions,
% or expand human_readable typeclass to include an additional method
% that takes options
:- type bool_opts ---> bool_opts(bool).
:- type of_seq ---> of_seq(solved_equation).
:- type solved_equation_with_print_opts ---> popts(bool_opts, of_seq).
% ^^^ this hiddeous thing is cuz' I couldn't declare it with type variables:
%      like this:  :- type with_print_opts(Opts, Of) ---> popts(Opts, Of).
%  and then do:
%      instance human_readable(with_print_opts(bool, solved_equation))
%  that was the stuff I first wanted.
%  That is becouse, AFAIU the parameter of_seq the instance it's a
%  "lvalue" of_seq type declaration, so it must match perfectly with the left hand of the type definition.
% At most, you could do:
%         'instance human_readable(with_print_opts(Var1, Var2))
% but that is another set of PIA compiler errors
%% plz ignore this design, is bad, but I'm just learning the ropes seeing what can be done, and having fun.

:- instance human_readable(solved_equation_with_print_opts) where [
    (to_string(popts(bool_opts(yes), of_seq(single_val(X)))) = string.format("%i", [i(X)])),

    (to_string(popts(bool_opts(no), of_seq(single_val(X)))) = to_string(popts(bool_opts(yes), of_seq(single_val(X))))),

    (to_string(popts(bool_opts(yes), of_seq(result_tree({R, {E1, Op, E2}})))) =
        string.format("%i = %s",
             [i(R), s(to_string(popts(bool_opts(no), of_seq(result_tree({R, {E1, Op, E2}})))))])),

    (to_string(popts(bool_opts(no), of_seq(result_tree({_, {E1, Op, E2}})))) =
        string.format("(%s %s %s)",
            [
               s(to_string(popts(bool_opts(no), of_seq(E1)))),
               s(to_string(Op)),
               s(to_string(popts(bool_opts(no), of_seq(E2))))
            ]))
].

% and some sane default behaviour
:- instance human_readable(solved_equation) where [
    (to_string(T) = to_string(popts(bool_opts(no), of_seq(T))))
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                %%%
%%%                          input parsing                         %%%
%%%                                                                %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred digit(char::out, list(char)::in, list(char)::out) is semidet.

digit(Char) -->
       [Char],
      { char.is_digit(Char) }.

:- pred integer_const(list(char)::out, list(char)::in, list(char)::out) is semidet.

integer_const([Digit|Rest]) -->
       digit(Digit),
       ( if integer_const(Const) then
           { Rest = Const }
       else
           { Rest = [] }
       ).

:- pred integer_dcg(int::out, list(char)::in, list(char)::out) is semidet.

integer_dcg(Int) --> integer_const(ConstInt),
                     { string.from_char_list(ConstInt, ConstString) },
                     { string.to_int(ConstString, Int) }.

:- pred numbers_sep_by_spaces(list.list(int)::out, list(char)::in, list(char)::out) is semidet.

% using if/then/else makes it *det, if you do pattern matching here, it will be multi and is not something I want.
numbers_sep_by_spaces([Number|Rest]) --> integer_dcg(Number),
                                         (if [' ']
                                          then numbers_sep_by_spaces(Rest)
                                          else { Rest = [] }).

:- pred equation_dcg(equation_numbers::out, list(char)::in, list(char)::out) is semidet.

equation_dcg(equats(Result, Numbers)) --> integer_dcg(Result), [':', ' '], numbers_sep_by_spaces(Numbers).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                %%%
%%%                              main                              %%%
%%%                                                                %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- func example = list.list(equation_numbers) is det.
example =
    [equats(190, [10, 19])
    ,equats(3267, [81, 40, 27])
    ,equats(83, [17, 5])
    ,equats(156, [15, 6])
    ,equats(7290, [6, 8, 6, 15])
    ,equats(161011, [16, 10, 13])
    ,equats(192, [17, 8, 14])
    ,equats(21037, [9, 7, 18, 13])
    ,equats(292, [11, 6, 16, 20])].


:- pred print_simple_test_for(equation_numbers::in, io::di, io::uo) is cc_multi.
print_simple_test_for(Eq, !IO) :-
    (if solve(Eq, Sol)
     then io.format(
        "%s -> %s\n",
        [s(to_string(Eq)), s(to_string(Sol))],
        !IO)
     else io.format("%s no solution found!\n", [s(to_string(Eq))], !IO)).

:- pred simple_tests(io::di, io::uo) is cc_multi.
simple_tests(!IO) :-
    print_simple_test_for(equats(9, [1,2,3,4]), !IO),
    print_simple_test_for(equats(13, [1,2,3,4]), !IO).


:- pred numbers_calResult_report(list.list(equation_numbers)::in, int::out, string::out) is cc_multi.
numbers_calResult_report(Numbers, CalibrationResult, Report) :-
    foldl((pred(Eq::in, Acc::in, NAcc::out) is multi :-
        {X0, Msg0} = Acc, EqStr = s(to_string(Eq)),
        (if solve(Eq, Sol)
        then
            Msg = string.format(
                "%s -> %s\n",
                [EqStr, s(to_string(Sol))]
            ),
                NAcc = {value(Sol) + X0, append(Msg0, Msg)}
            else
                NAcc = {X0,
                append(Msg0,
                    string.format("nopes! %s has no solution :'(\n", [EqStr])
                )}
            )
        )
        , Numbers, {0, ""}, {CalibrationResult, Report}).

main(!IO) :-

    io.format("\n\n%i\n\n", [i(1+det_to_int(string(11) ++ string(2)))], !IO),

    %% leave it here for doc purposes, see comment in operator_result_in_int(conc,...)
    % operator_result_int_int(_, _, 1, 2),

    io.write_string("Ok, let's try to solve this!\nbut, first the test!\n" ++
                    "some simple tests...\n"
                   , !IO),

    simple_tests(!IO),

    io.write_string("testing the example\n", !IO),

    numbers_calResult_report(example, ExampleResult, ExampleReport),

   (if (ExampleResult = 3749)
    then io.write_string("\nexample yields part1 result!\n\n", !IO)
    else (if (ExampleResult = 11387)
          then io.write_string("\nexample yields part2 result!\n\n", !IO))
          else throw("Tests failed!")),

    io.format("details:\n%s\ntotal calibration result: %i\n", [s(ExampleReport), i(ExampleResult)], !IO),

    io.command_line_arguments(Args, !IO),
    ( if Args = [InputFileName] then
        io.open_input(InputFileName, OpenResult, !IO),
        (
            OpenResult = ok(InputFile),
            io.read_file_as_string(InputFile, ReadResult, !IO),
            (
                ReadResult = ok(StartString),
                Lines = split_into_lines(StartString),
                ( if map((pred(Line::in, Equation::out) is semidet :-
                       equation_dcg(Equation, to_char_list(Line), [])), Lines, Numbers)
                  then
                     numbers_calResult_report(Numbers, Result, Report),
                     io.format("%s", [s(Report)], !IO),
                     io.format("total calibration result: %i\n", [i(Result)], !IO)
                  else
                     io.write_string("couldn't read the input!\n", !IO)
                )
            ;
                ReadResult = error(_, Error),
                io.write_string(io.error_message(Error), !IO),
                io.nl(!IO),
                io.set_exit_status(1, !IO)
            )
        ;
            OpenResult = error(Error),
            io.write_string(io.error_message(Error), !IO),
            io.nl(!IO),
            io.set_exit_status(1, !IO)
        )
    else
        io.write_string("\nusage: day7 <filename>\n", !IO),
        io.set_exit_status(1, !IO)
    ).
