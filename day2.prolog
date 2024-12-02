#!/usr/bin/env swipl

%% --- Day 2: Red-Nosed Reports ---

%% Fortunately, the first location The Historians want to search isn't a long walk from the Chief Historian's office.

%% While the Red-Nosed Reindeer nuclear fusion/fission plant appears to contain no sign of the Chief Historian, the engineers there run up to you as soon as they see you. Apparently, they still talk about the time Rudolph was saved through molecular synthesis from a single electron.

%% They're quick to add that - since you're already here - they'd really appreciate your help analyzing some unusual data from the Red-Nosed reactor. You turn to check if The Historians are waiting for you, but they seem to have already divided into groups that are currently searching every corner of the facility. You offer to help with the unusual data.

%% The unusual data (your puzzle input) consists of many reports, one report per line. Each report is a list of numbers called levels that are separated by spaces. For example:

%% 7 6 4 2 1
%% 1 2 7 8 9
%% 9 7 6 2 1
%% 1 3 2 4 5
%% 8 6 4 4 1
%% 1 3 6 7 9

%% This example data contains six reports each containing five levels.

%% The engineers are trying to figure out which reports are safe. The Red-Nosed reactor safety systems can only tolerate levels that are either gradually increasing or gradually decreasing. So, a report only counts as safe if both of the following are true:

%%     The levels are either all increasing or all decreasing.
%%     Any two adjacent levels differ by at least one and at most three.

%% In the example above, the reports can be found safe or unsafe by checking those rules:

%%     7 6 4 2 1: Safe because the levels are all decreasing by 1 or 2.
%%     1 2 7 8 9: Unsafe because 2 7 is an increase of 5.
%%     9 7 6 2 1: Unsafe because 6 2 is a decrease of 4.
%%     1 3 2 4 5: Unsafe because 1 3 is increasing but 3 2 is decreasing.
%%     8 6 4 4 1: Unsafe because 4 4 is neither an increase or a decrease.
%%     1 3 6 7 9: Safe because the levels are all increasing by 1, 2, or 3.

%% So, in this example, 2 reports are safe.

%% Analyze the unusual data from the engineers. How many reports are safe?

:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

%:- portray_text(true).

spaces --> ` ` | ` `, spaces.
maybe_spaces --> `` | spaces.
maybe_newline --> `` | `\n`.
level(L) --> integer(L).
report_line([L|Lvls]) --> maybe_spaces, level(L), spaces, report_line(Lvls).
report_line([L]) --> maybe_spaces, level(L), maybe_spaces.
reports([R]) --> report_line(R), maybe_newline.
reports([R|Rps]) --> report_line(R), `\n`, reports(Rps).

%% Testing DCG:
:- phrase(spaces, `   `, ``),
   phrase(maybe_spaces, ``, ``),
   phrase(maybe_newline, `\n`,``).
:- phrase(report_line(_), ` 1 2 3 `, ``),
   phrase(report_line(_), `1 2 3 `, ``),
   phrase(report_line(_), ` 1 2 3`, ``).
:- phrase(reports(Reports),
          `7 6 4 2 1
           1 2 7 8 9
           9 7 6 2 1
           1 3 2 4 5
           8 6 4 4 1
           1 3 6 7 9`, _),
   nth1(1, Reports, [7, 6, 4, 2, 1], _),
   nth1(6, Reports, [1, 3, 6, 7, 9], _).

% Ok, parser done

numbers_consecutiveDifferences([X, Y], [D]) :- D is Y - X.
numbers_consecutiveDifferences([X0|[X1|Xs]], [D|Dfs]) :- D is X1 - X0, numbers_consecutiveDifferences([X1|Xs], Dfs).

all_positive([]).
all_positive([X|Xs]) :- X >= 0, all_positive(Xs).
all_negative([]).
all_negative([X|Xs]) :- X < 0, all_negative(Xs).
monotonic(Xs) :- all_negative(Xs).
monotonic(Xs) :- all_positive(Xs).

list_moduloList(Ls, Mls) :- maplist([X,S]>>(S is abs(X)), Ls, Mls).

safeReport(R) :- numbers_consecutiveDifferences(R, D), monotonic(D), list_moduloList(D, Dpos), forall(member(X, Dpos), (X > 0, X < 4)).
count(_, [], 0).
count(UnaryGoal, [X|Xs], M) :- call(UnaryGoal, X), !, count(UnaryGoal, Xs, N), M is N + 1.
count(UnaryGoal, [_|Xs], N) :- count(UnaryGoal, Xs, N).

% ok, testing from the problem statement.
% 7 6 4 2 1: Safe because the levels are all decreasing by 1 or 2.
:- safeReport([7, 6, 4, 2, 1]).
% 1 2 7 8 9: Unsafe because 2 7 is an increase of 5.
:- phrase(report_line(R), `1 2 7 8 9`), \+ safeReport(R).
% 9 7 6 2 1: Unsafe because 6 2 is a decrease of 4.
:- phrase(report_line(R), `9 7 6 2 1`), \+ safeReport(R).
% 1 3 2 4 5: Unsafe because 1 3 is increasing but 3 2 is decreasing.
:- phrase(report_line(R), `1 3 2 4 5`), \+ safeReport(R).
% 8 6 4 4 1: Unsafe because 4 4 is neither an increase or a decrease.
:- phrase(report_line(R), `8 6 4 4 1`), \+ safeReport(R).
% 1 3 6 7 9: Safe because the levels are all increasing by 1, 2, or 3.
:- phrase(report_line(R), `1 3 6 7 9`), safeReport(R).

%% --- Part Two ---

%% The engineers are surprised by the low number of safe reports until they realize they forgot to tell you about the Problem Dampener.

%% The Problem Dampener is a reactor-mounted module that lets the reactor safety systems tolerate a single bad level in what would otherwise be a safe report. It's like the bad level never happened!

%% Now, the same rules apply as before, except if removing a single level from an unsafe report would make it safe, the report instead counts as safe.

%% More of the above example's reports are now safe:

%%     7 6 4 2 1: Safe without removing any level.
%%     1 2 7 8 9: Unsafe regardless of which level is removed.
%%     9 7 6 2 1: Unsafe regardless of which level is removed.
%%     1 3 2 4 5: Safe by removing the second level, 3.
%%     8 6 4 4 1: Safe by removing the third level, 4.
%%     1 3 6 7 9: Safe without removing any level.

%% Thanks to the Problem Dampener, 4 reports are actually safe!

%% Update your analysis by handling situations where the Problem Dampener can remove a single level from unsafe reports. How many reports are now safe?

dampenedSafeReport(R) :- dampenedSafeReport(R,_).
dampenedSafeReport(R, none) :- safeReport(R).
dampenedSafeReport(R, X) :- member(X, R), append(Head, [X|Tail], R), append(Head, Tail, DampenedReport), safeReport(DampenedReport).

% testing it - ver para creer:
%%     7 6 4 2 1: Safe without removing any level.
:- phrase(report_line(R), `7 6 4 2 1`), dampenedSafeReport(R, _).
%%     1 2 7 8 9: Unsafe regardless of which level is removed.
:- phrase(report_line(R), `1 2 7 8 9`), \+ dampenedSafeReport(R, _).
%%     9 7 6 2 1: Unsafe regardless of which level is removed.
:- phrase(report_line(R), `9 7 6 2 1`), \+ dampenedSafeReport(R, _).
%%     1 3 2 4 5: Safe by removing the second level, 3.
:- phrase(report_line(R), `1 3 2 4 5`), dampenedSafeReport(R, _).
%%     8 6 4 4 1: Safe by removing the third level, 4.
:- phrase(report_line(R), `8 6 4 4 1`), dampenedSafeReport(R, _).
%%     1 3 6 7 9: Safe without removing any level.
:- phrase(report_line(R), `1 3 6 7 9`), dampenedSafeReport(R, _).

% IO
:- initialization(main, main).
main([File]) :-
    phrase_from_file(reports(Rps), File),
    length(Rps, L),
    count(safeReport, Rps, SafeReportsCount), % part I
    count(dampenedSafeReport, Rps, DampenedSafeReportsCount), % part II
    writef("archivo: %w con %w reportes\n%w -> reportes seguros.\n%w -> reportes seguros con tolerancia\n",
           [File, L, SafeReportsCount, DampenedSafeReportsCount]).
