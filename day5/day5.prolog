#!/usr/bin/env swipl

:- portray_text(true).

:- use_module(etrace).

:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                %%%
%%%                              UTILs                             %%%
%%%                                                                %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remove_from_rest(X, Xs, Rest) :- nth0(_, Xs, X, R), !, remove_from_rest(X, R, Rest).
remove_from_rest(X, Xs, Xs) :- \+member(X, Xs).

list_set([], []).
list_set([X|Xs], [X|S]) :- remove_from_rest(X, Xs, R), list_set(R, S).

%% https://adventofcode.com/2024/day/5
% --- Day 5: Print Queue ---

% Satisfied with their search on Ceres, the squadron of scholars suggests subsequently scanning the stationery stacks of sub-basement 17.

% The North Pole printing department is busier than ever this close to Christmas, and while The Historians continue their search of this historically significant facility, an Elf operating a very familiar printer beckons you over.

% The Elf must recognize you, because they waste no time explaining that the new sleigh launch safety manual updates won't print correctly. Failure to update the safety manuals would be dire indeed, so you offer your services.

% Safety protocols clearly indicate that new pages for the safety manuals must be printed in a very specific order. The notation X|Y means that if both page number X and page number Y are to be produced as part of an update, page number X must be printed at some point before page number Y.

% The Elf has for you both the page ordering rules and the pages to produce in each update (your puzzle input), but can't figure out whether each update has the pages in the right order.

% For example:

% 47|53
% 97|13
% 97|61
% 97|47
% 75|29
% 61|13
% 75|53
% 29|13
% 97|29
% 53|29
% 61|53
% 97|53
% 61|29
% 47|13
% 75|47
% 97|75
% 47|61
% 75|61
% 47|29
% 75|13
% 53|13

% 75,47,61,53,29
% 97,61,53,29,13
% 75,29,13
% 75,97,47,61,53
% 61,13,29
% 97,13,75,29,47

% The first section specifies the page ordering rules, one per line. The first rule, 47|53, means that if an update includes both page number 47 and page number 53, then page number 47 must be printed at some point before page number 53. (47 doesn't necessarily need to be immediately before 53; other pages are allowed to be between them.)

% The second section specifies the page numbers of each update. Because most safety manuals are different, the pages needed in the updates are different too. The first update, 75,47,61,53,29, means that the update consists of page numbers 75, 47, 61, 53, and 29.

% To get the printers going as soon as possible, start by identifying which updates are already in the right order.

% In the above example, the first update (75,47,61,53,29) is in the right order:

%     75 is correctly first because there are rules that put each other page after it: 75|47, 75|61, 75|53, and 75|29.
%     47 is correctly second because 75 must be before it (75|47) and every other page must be after it according to 47|61, 47|53, and 47|29.
%     61 is correctly in the middle because 75 and 47 are before it (75|61 and 47|61) and 53 and 29 are after it (61|53 and 61|29).
%     53 is correctly fourth because it is before page number 29 (53|29).
%     29 is the only page left and so is correctly last.

% Because the first update does not include some page numbers, the ordering rules involving those missing page numbers are ignored.

% The second and third updates are also in the correct order according to the rules. Like the first update, they also do not include every page number, and so only some of the ordering rules apply - within each update, the ordering rules that involve missing page numbers are not used.

% The fourth update, 75,97,47,61,53, is not in the correct order: it would print 75 before 97, which violates the rule 97|75.

% The fifth update, 61,13,29, is also not in the correct order, since it breaks the rule 29|13.

% The last update, 97,13,75,29,47, is not in the correct order due to breaking several rules.

% For some reason, the Elves also need to know the middle page number of each update being printed. Because you are currently only printing the correctly-ordered updates, you will need to find the middle page number of each correctly-ordered update. In the above example, the correctly-ordered updates are:

% 75,47,61,53,29
% 97,61,53,29,13
% 75,29,13

% These have middle page numbers of 61, 53, and 29 respectively. Adding these page numbers together gives 143.

% Of course, you'll need to be careful: the actual list of page ordering rules is bigger and more complicated than the above example.

% Determine which updates are already in the correct order. What do you get if you add up the middle page number from those correctly-ordered updates?

% super easy in prolog
update_rule(Upd, X-Y) :- nth0(I, Upd, X), nth0(J, Upd, Y), I #< J, !.
update_rule(Upd, X-Y) :- \+ member(X, Upd) ; \+ member(Y, Upd).

% :- update_rule([1,2,3], 1-3).
%@ true.
% :- update_rule([3,2,1], 1-3).
%@ false.
% :- update_rule([1,2,3], 1-4).
%@ true.
% :- update_rule([1,2,3], 0-2).
%@ true

correctUpdate_rules(U, Rs) :- forall(member(R, Rs), update_rule(U, R)).

% :- correctUpdate_rules([3,1], []).
%@ true.
% :- correctUpdate_rules([3,1], [1-2,2-3]).
%@ true.
% :- correctUpdate_rules([1,3], [1-2,2-3]).
%@ true.
% :- correctUpdate_rules([3,1,2], [1-2,2-3]).
%@ false.
% :- correctUpdate_rules([2,3,1], [1-2,2-3]).
%@ false.
% :- correctUpdate_rules([1,2,3], [1-2,2-3]).
%@ true.
% :- correctUpdate_rules([1,2,3,4], [1-2,2-3]).

updates_rules_correctUpdates(Upds, Rs, Oks) :-
    findall(U, (member(U, Upds),
                correctUpdate_rules(U, Rs))
            ,Oks).

% :- updates_rules_correctUpdates([[1,2,3], [1,3,2]], [1-3,2-3], C).
%@ C = [[1, 2, 3]].

middlePage_ofUpdate(M, Up) :- append([H,[M],T], Up), length(H, N), length(T, N).


rule(X-Y) --> integer(X), `|`, integer(Y), `\n`.
                                           update([Page])       --> integer(Page).
update([Page|Pages]) --> integer(Page), `,`, update(Pages).

input_rules([R]) --> rule(R).
input_rules([R|Rs]) --> rule(R), input_rules(Rs).
input_updates([U]) --> update(U).
input_updates([U|Ups]) --> update(U), `\n`, input_updates(Ups).
input(Rs, Ups) --> input_rules(Rs), `\n`, input_updates(Ups).

:- phrase(rule(23-43), `23|43\n`).
:- phrase(input_rules([23-43]), `23|43\n`).
:- phrase(input_rules([23-43,45-64]), `23|43\n45|64\n`).

example(
    `47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47`).

updates_middlePageSum(Updates, Result) :-
    findall(M,
            (   member(C, Updates),
                middlePage_ofUpdate(M, C)
            ),
            Middles),
    sum_list(Middles, Result).

rules_updates_result(Rs, Ups, Result) :-
    updates_rules_correctUpdates(Ups, Rs, Correct),
    updates_middlePageSum(Correct, Result).

exampleRules_updates(Rs, Ups) :- example(Cs), phrase(input(Rs, Ups), Cs).

:- exampleRules_updates(Rs, Ups),
   member(47-53, Rs), member(61-53, Rs), member(53-13, Rs),
   member([75,47,61,53,29], Ups), member([75,29,13], Ups),
   member([97,13,75,29,47], Ups),
   rules_updates_result(Rs, Ups, 143).


% --- Part Two ---

% While the Elves get to work printing the correctly-ordered updates, you have a little time to fix the rest of them.

% For each of the incorrectly-ordered updates, use the page ordering rules to put the page numbers in the right order. For the above example, here are the three incorrectly-ordered updates and their correct orderings:

% 75,97,47,61,53 becomes 97,75,47,61,53.
% 61,13,29 becomes 61,29,13.
% 97,13,75,29,47 becomes 97,75,47,29,13.

% After taking only the incorrectly-ordered updates and ordering them correctly, their middle page numbers are 47, 29, and 47. Adding these together produces 123.

% Find the updates which are not in the correct order. What do you get if you add up the middle page numbers after correctly ordering just those updates?

element_beforeThanElement_inList(A, B, L) :-
    nth0(I, L, A), nth0(J, L, B), I #< J.

% this works, but is damn slow ... it won't terminate for the problem input
incorrect_rules_corrected__slow(W, Rs, Ok) :-
    length(W, L), length(Ok, L),
    foreach((member(A-B, Rs), member(A, W), member(B, W)),
            element_beforeThanElement_inList(A, B, Ok)),
    foreach(member(X, W), member(X, Ok)).

incorrects_rules_correctedList__slow(Wrongs, Rs, Oks) :-
    findall(Ok,
            (   member(W, Wrongs),
                incorrect_rules_corrected__slow(W, Rs, Ok))
            , Oks).

% +Ups, +CorrectLst, -Incorrects
updates_corrects_incorrects(Ups, CorrectLst, Incorrects) :-
    findall(I, (member(I, Ups), \+ member(I, CorrectLst)), Incorrects).


% but it works for the example:
%% :- exampleRules_updates(Rs, Ups),
%%    updates_rules_correctUpdates(Ups, Rs, CorrectLst),
%%    updates_corrects_incorrects(Ups, CorrectLst, Incorrects),
%%    member(W, Incorrects),
%%    incorrect_rules_corrected__slow(W, Rs, Ok),
%%    nodes_dagEdges_sortedNodes(W, Rs, Ok).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                %%%
%%%                     Directed Acyclic GRAPHs                    %%%
%%%                                                                %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

graphEmpty(loe([])).

% so let's see if we could pull off a top sort with dag in prolog

nodes_graph(Xs, loe(E)) :- setof(X, node_graph_(X, loe(E)), Xs).
node_graph_(X, loe(E)) :- member(X-_, E) ; member(_-X, E).
node_graph(X, G) :- nodes_graph(Ns, G), member(X, Ns).

edge(E, loe(Es)) :- member(E, Es).
edge(E, loe(Es), Rest) :- nth0(_, Es, E, Rest).

path_from_to_dag([X, Y], X, Y, loe(Es)) :- edge(X-Y, loe(Es)).
path_from_to_dag(Path, X, Z, loe(Es)) :-
    edge(X-Y, loe(Es), NUE),
    path_from_to_dag(P1, Y, Z, loe(NUE)),
    [X|P1]=Path.
path_from_to_dag(_, X, Y, loe([])) :- dif(X,Y), fail.

% :- path_from_to_dag(P, 1, 4, loe([1-2,1-3,3-5,3-4,5-4])).
%@ P = [1, 3, 4] ;
%@ P = [1, 3, 5, 4] ;
%@ false.
% :- path_from_to_dag(P, X, 4, loe([1-2,1-3,3-5,3-4,5-4])).
%@ P = [3, 4],
%@ X = 3 ;
%@ P = [5, 4],
%@ X = 5 ;
%@ P = [1, 3, 4],
%@ X = 1 ;
%@ P = [1, 3, 5, 4],
%@ X = 1 ;
%@ P = [3, 5, 4],
%@ X = 3 ;
%@ false.

% it can be used to get neighbours
% :- path_from_to_dag([1, X], 1, X, loe([1-2,1-3,3-5,3-4,5-4])).
%@ X = 2 ;
%@ X = 3 ;
%@ false.

% or outgoin edges:
% :- path_from_to_dag(EdgeL, 1, X, loe([1-2,1-3,3-5,3-4,5-4])), EdgeL=[1, X], Edge=1-X.
%@ EdgeL = [1, 2],
%@ X = 2,
%@ Edge = 1-2 ;
%@ EdgeL = [1, 3],
%@ X = 3,
%@ Edge = 1-3 ;
%@ false.

node_exit_graph(N, E, loe(Es)) :- path_from_to_dag(EdgeL, N, X, loe(Es)), EdgeL=[N, X], E=1-X.

node_adyacent_graph(N, M, loe(Es)) :- path_from_to_dag([N,M], N, M, loe(Es)).

subGraph_edge_graph(loe(SErest), E, loe(SEs)) :- nth0(_,SEs,E,SErest).

subGraph_node_graph(loe(SEs), N, loe(Es)) :-
    node_graph(N, loe(Es)),
    findall(X, (member(X, Es), \+ (X=N-_ ; X=_-N)), SEs).

sourceNode_graph(N, loe(Es)) :-
    nodes_graph(Ns, loe(Es)), member(N, Ns),
    \+ path_from_to_dag(_, _, N, loe(Es)).

% :- sourceNode_graph(Xs, loe([1-2,1-3,3-5,3-4,5-4])).
%@ Xs = 1 ;
%@ false.
% :- sourceNode_graph(Xs, loe([1-2,1-3,3-4,5-4])).
%@ Xs = 1 ;
%@ Xs = 5.

sinkNode_graph(_, loe([])).
sinkNode_graph(N, loe(Es)) :-
    nodes_graph(Ns, loe(Es)), member(N, Ns),
    \+ path_from_to_dag(_, N, _, loe(Es)).

depthFirstTraversal_fromNode_dag(T, A, G) :-
    sinkNode_graph(Z, G), path_from_to_dag(T, A, Z, G).

depthFirstTraversals_fromNode_dag(Ts, A, G) :-
    findall(T, depthFirstTraversal_fromNode_dag(T, A, G), Ts).

% :- depthFirstTraversal_fromNode_dag(T, 1, loe([1-2,1-3,3-5,3-4,5-4])).
%@ T = [1, 2] ;
%@ T = [1, 3, 4] ;
%@ T = [1, 3, 5, 4] ;
%@ false.

nodeInDepthFirstTraversal_fromNode_dag(X, A, G) :-
    depthFirstTraversals_fromNode_dag(TTs, A, G),
    append(TTs, TTS),
    list_set(TTS, Ts), member(X, Ts).

% :- nodeInDepthFirstTraversal_fromNode_dag(T, 1, loe([1-2,1-3,3-5,3-4,5-4])).
%@ T = 1 ;
%@ T = 2 ;
%@ T = 3 ;
%@ T = 4 ;
%@ T = 5 ;
%@ false.

% :- nodeInDepthFirstTraversal_fromNode_dag(T, 1, loe([1-2,1-3,1-4,2-6,2-7])).
%@ T = 1 ;
%@ T = 3 ;
%@ T = 4 ;
%@ T = 2 ;
%@ T = 6 ;
%@ T = 7 ;
%@ false.


% for the lulz, is not used:
nodeInBreathFirstTraversal_fromNode_dag_(N, N, G) :- nodes_graph(Xs, G), member(N, Xs).
nodeInBreathFirstTraversal_fromNode_dag_(N, S, G) :- \+ sinkNode_graph(S, G), node_adyacent_graph(S, M, G), nodeInBreathFirstTraversal_fromNode_dag_(N, M, G).

nodeInBreathFirstTraversal_fromNode_dag(X, S, G) :-
    findall(N, nodeInBreathFirstTraversal_fromNode_dag_(N, S, G), XXs),
    list_set(XXs, Xs), member(X, Xs).

% :- nodeInBreathFirstTraversal_fromNode_dag(X, 1, loe([1-2,1-3,3-5,3-4,5-4])).
%@ X = 1 ;
%@ X = 2 ;
%@ X = 3 ;
%@ X = 5 ;
%@ X = 4 ;
%@ false.

% -Gs, +E, -S, +S, +G
subGraph_sourceNodesOut_edges_sourceNodesIn_graph(G, S, [], S, G).
subGraph_sourceNodesOut_edges_sourceNodesIn_graph(Gs, Sout, [E|Es], Sin, G) :-
    subGraph_edge_graph(Gs0, E, G), E=_-M,
    (   (   sourceNode_graph(M, Gs0) ; graphEmpty(Gs0) ) -> Sout = [M|SoutR]
    ;   SoutR = Sout),
    subGraph_sourceNodesOut_edges_sourceNodesIn_graph(Gs, SoutR, Es, Sin, Gs0).

% https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm
khanToposorted_dag(L, G) :-
    findall(S,sourceNode_graph(S, G),Ss), khanToposorted_dag_(L, G, Ss).
khanToposorted_dag_([], loe([]), []).
khanToposorted_dag_([N|L], G, S) :- dif(S,[]),
                                    nth0(_, S, N, Sr),
                                    findall(N-M, node_adyacent_graph(N, M, G), Neights),
                                    subGraph_sourceNodesOut_edges_sourceNodesIn_graph(Gs, SrR, Neights, Sr, G),
                                    khanToposorted_dag_(L, Gs, SrR).

% :- khanToposorted_dag(L, loe([1-2,2-4,1-3,3-4])).
%@ L = [1, 2, 3, 4] ;
%@ L = [1, 3, 2, 4] ;
%@ false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                %%%
%%%                  Let's use it for the problem                  %%%
%%%                                                                %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

elem_in(X, [X|_]) :- !.
elem_in(X, [Y|Lst]) :- dif(X, Y), elem_in(X, Lst).

update_rules_graph(Up, Rs, loe(Es)) :-
    % luckly there aren't repeated numbers, so it's easiest
    findall(X-Y, (member(X-Y, Rs), elem_in(X, Up), elem_in(Y, Up)), Es0),
    findall(source-S, (sourceNode_graph(S, loe(Es0)),
                       elem_in(S, Up)), Ss),
    append([Ss,Es0], Es).

incorrect_rules_corrected__fast(Up, Rs, Ok) :-
    update_rules_graph(Up, Rs, G),
    khanToposorted_dag(Xs, G),
    [source|Traversal] = Xs,
    findall(X, (member(X, Up), \+member(X, Traversal)), Rest),
    append([Rest,Traversal], Ok). % <- TODO, here we can have more orderings that prepend Rest!

% incorrect_rules_corrected__fast([5,4,3,2,1,6], [0-2,1-2,1-3,3-5,3-4,5-4], C)

incorrects_rules_correctedList__fast(Wrongs, Rs, Oks) :-
    findall(Ok,
            (   member(W, Wrongs),
                writef("trying to solve: %w", [W]), nl,
                incorrect_rules_corrected__fast(W, Rs, Ok),
                writef("solved: %w", [Ok]), nl)
            , Oks).

:- exampleRules_updates(Rs, Ups),
   updates_rules_correctUpdates(Ups, Rs, CorrectLst),
   updates_corrects_incorrects(Ups, CorrectLst, Incorrects),
   incorrects_rules_correctedList__slow(Incorrects, Rs, Corrected),
   incorrects_rules_correctedList__fast(Incorrects, Rs, CorrectedFast),
   updates_middlePageSum(Corrected, 123),
   updates_middlePageSum(CorrectedFast, 123).


% slow implementation doesn't even terminate with largest W ...
% :- W = [71,52,34,31,94,16,24,58,53,55,87,41,23,28,96,84,13,32,85,29,42,97,57],
%    %W = [71,52,34,41,23,28,96,84,13,32,85,29],
%    % W=[75,97,47,61,53],
%    exampleRules_updates(Rs, _),
%    \+ correctUpdate_rules(W, Rs),
%    writef("is wrong:       %w ",[W]),nl,
%    writef("rules:          %w", [Rs]),nl,
%    update_rules_graph(W, Rs, G),
%    writef("graph:          %w", [G]),nl,
%    %incorrect_rules_corrected__slow(W, Rs, Ok),
%    %writef("corrected slow: %w", [Ok]),nl, %trace,
%    incorrect_rules_corrected__fast(W, Rs, OkF),
%    writef("corrected fast: %w", [OkF]),nl,
%    %correctUpdate_rules(Ok, Rs),
%    %write("slow works ok!"),nl,
%    correctUpdate_rules(OkF, Rs),
%    write("fast works ok!"),nl,
%    %permutation(W, Ok),
%    permutation(W, OkF).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                %%%
%%%                            MAIN / IO                           %%%
%%%                                                                %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- initialization(main, main).
main([File]) :-
    phrase_from_file(input(Rs, Ups), File), !,
    writef("archivo: %w\n", [File]),
    updates_rules_correctUpdates(Ups, Rs, CorrectLst),
    updates_middlePageSum(CorrectLst, OkResult),
    writef("la suma de las páginas medianas da: %w\n",
           [OkResult]),
    updates_corrects_incorrects(Ups, CorrectLst, Incorrects),
    incorrects_rules_correctedList__fast(Incorrects, Rs, Corrected),
    updates_middlePageSum(Corrected, CorrectedResult),
    writef("la suma de las medianas de los updates corregidos da: %w\n",
           [CorrectedResult]).
