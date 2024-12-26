:- module day7.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.
:- implementation.
:- import_module list, string, int.


% integer_dcg([]).
% integer_dcg([X|Xs]) --> ( X=0; X=1; X=2; X=3; X=4; X=5; X=6; X=7; X=8; X=9 ), integer_dcg(Xs).

% numbers([X|Xs]) --> integer_dcg(X).
% numbers([X|Xs]) --> integer_dcg(X), ' ', numbers(Xs).

% equation_dcg(Result, Numbers) --> integer_dcg(Result), ': ', numbers_dcg(Numbers).

:- pred example(list.list({int, list.list(int)})::out) is det.
example(
    [{190, [10, 19]}
    ,{3267, [81, 40, 27]}
    ,{83, [17, 5]}
    ,{156, [15, 6]}
    ,{7290, [6, 8, 6, 15]}
    ,{161011, [16, 10, 13]}
    ,{192, [17, 8, 14]}
    ,{21037, [9, 7, 18, 13]}
    ,{292, [11, 6, 16, 20]}]).

:- type operator ---> plus ; mult.

:- pred operator_int_int_result(operator::out, int::in, int::in, int::in).
operator_int_int_result(plus, X, Y, X + Y).
operator_int_int_result(mult, X, Y, X * Y).

:- func operator_string(operator) = string.
operator_string(plus) = "+".
operator_string(mult) = "x".

main(!IO) :-
    io.write_string("Hello, world!\n", !IO),
    %example([Z,[X,Y]]|_]),
    (if operator_int_int_result(Op, 2, 1, 3)
    then io.format("the first operator is %s\n", [s(operator_string(Op))], !IO)
    else io.write_string("F\n", !IO)).
%io.open_input("day7.input", Stream, !IO),
%    io.format("%s", )
    
