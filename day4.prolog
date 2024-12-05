#!/usr/bin/env swipl

:- portray_text(true).

:- use_module(library(clpfd)).

%% --- Day 4: Ceres Search ---

%% "Looks like the Chief's not here. Next!" One of The Historians pulls out a device and pushes the only button on it. After a brief flash, you recognize the interior of the Ceres monitoring station!

%% As the search for the Chief continues, a small Elf who lives on the station tugs on your shirt; she'd like to know if you could help her with her word search (your puzzle input). She only has to find one word: XMAS.

%% This word search allows words to be horizontal, vertical, diagonal, written backwards, or even overlapping other words. It's a little unusual, though, as you don't merely need to find one instance of XMAS - you need to find all of them. Here are a few ways XMAS might appear, where irrelevant characters have been replaced with .:

%% ..X...
%% .SAMX.
%% .A..A.
%% XMAS.S
%% .X....

%% The actual word search will be full of letters instead. For example:

%% MMMSXXMASM
%% MSAMXMSMSA
%% AMXSXMAAMM
%% MSAMASMSMX
%% XMASAMXAMM
%% XXAMMXXAMA
%% SMSMSASXSS
%% SAXAMASAAA
%% MAMMMXMMMM
%% MXMXAXMASX

%% In this word search, XMAS occurs a total of 18 times; here's the same word search again, but where letters not involved in any XMAS have been replaced with .:

%% ....XXMAS.
%% .SAMXMS...
%% ...S..A...
%% ..A.A.MS.X
%% XMASAMX.MM
%% X.....XA.A
%% S.S.S.S.SS
%% .A.A.A.A.A
%% ..M.M.M.MM
%% .X.X.XMASX

%% Take a look at the little Elf's word search. How many times does XMAS appear?


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                %%%
%%%                 AUX, List and Matrix predicates                %%%
%%%                 not a bad candidate for library                %%%
%%%                                                                %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

element_list_count(_, [], 0).
element_list_count(X, [X|Xs], C) :- element_list_count(X, Xs, N), C #= 1 + N.
element_list_count(Y, [X|Xs], C) :- dif(Y,X), element_list_count(Y, Xs, C).

element_matrix_count(_, [], 0).
element_matrix_count(X, [R|M], C) :- element_list_count(X, R, LC), C #= LC + MC, element_matrix_count(X, M, MC).

n_firstRows_ofMatrix(N, R, M) :- n_firstElements_ofList(N, R, M).
n_firstElements_ofList(N, Els, L) :- append([Els,_], L), length(Els, N).

n_upperTopSubmatrix_ofMatrix(N, S, M) :-
    n_firstRows_ofMatrix(N, Rs, M),
    findall(RowHead, (member(R, Rs), n_firstElements_ofList(N, RowHead, R)), S).

firstRow_bottomMatrix_ofMatrix(R, B, [R|B]).

firstColumn_ofMatrix_rightMatrix(C, M, RM) :-
    findall(H, member([H|_], M), C),
    findall(T, member([_|T], M), RM).

n_firstColumns_ofMatrix_rightMatrix(1, [Col], M, RM) :-
    firstColumn_ofMatrix_rightMatrix(Col, M, RM), !.
n_firstColumns_ofMatrix_rightMatrix(N, [Col|Cs], M, RM) :-
    N #> 1,
    firstColumn_ofMatrix_rightMatrix(Col, M, RM0),
    N0 #= N - 1,
    n_firstColumns_ofMatrix_rightMatrix(N0, Cs, RM0, RM).

% :- n_firstColumns_ofMatrix_rightMatrix(3, F,
%     [[1,2,3,4,5],
%      [6,7,8,9,0],
%      [2,3,4,5,6]], RM).
%@ F = [[1, 6, 2], [2, 7, 3], [3, 8, 4]],
%@ RM = [[4, 5], [9, 0], [5, 6]].

splitMatrix_atColumn_leftMatrix_rightMatrix(M, N, LM, RM):- n_firstColumns_ofMatrix_rightMatrix(N, C, M, RM), transpose(C, LM).
% :- splitMatrix_atColumn_leftMatrix_rightMatrix(
%     [[1,2,3,4,5],
%      [6,7,8,9,0],
%      [2,3,4,5,6]], 3, LM, RM).
%@ LM = [[1, 2, 3], [6, 7, 8], [2, 3, 4]],
%@ RM = [[4, 5], [9, 0], [5, 6]].

width_matrix(N, [X|_]) :- length(X, N).
height_matrix(N, M) :- length(M, N).
size_matrix(H-W, M) :- height_matrix(H, M), width_matrix(W, M).
sameSizedMatrix(N, M) :- height_matrix(H, N), height_matrix(H, M),
                         width_matrix(W, N), width_matrix(W, M).

matrix_verticalFlip(M, V) :- findall(RR, (member(R, M), reverse(R, RR)), V).

%:- matrix_verticalFlip([[1,2],[3,4]], V).
%@ V = [[2, 1], [4, 3]].

matrix_biggestProperPrincipal(M, RBM) :-
    firstRow_bottomMatrix_ofMatrix(_, BM, M),
    firstColumn_ofMatrix_rightMatrix(_, BM, RBM).

matrix_matrixPrincipal(M, M).
matrix_matrixPrincipal(M, P) :-
    matrix_biggestProperPrincipal(M, SM),
    matrix_matrixPrincipal(SM, P).

%:- matrix_matrixPrincipal([[1,2,3],[4,5,6],[7,8,9]], P).
%@ P = [[1, 2, 3], [4, 5, 6], [7, 8, 9]] ;
%@ P = [[5, 6], [8, 9]] ;
%@ P = [[9]] ;
%@ P = [] ;
%@ false.

matrix_elementOfPrincipalDiagonal(M, H) :- 
    size_matrix(N-N, M), between(0, N, I), nth0(I, M, Row, _), nth0(I, Row, H).

%:- matrix_elementOfPrincipalDiagonal([[1,2,3],[4,5,6],[7,8,9]], E).
%@ E = 1 ;
%@ E = 5 ;
%@ E = 9 ;
%@ false.

matrix_principalDiagonal(M, D) :- findall(E, matrix_elementOfPrincipalDiagonal(M, E), D).

%:- matrix_principalDiagonal([[1,2,3],[4,5,6],[7,8,9]], D).
%@ D = [1, 5, 9].

matrix_diagonals(M, [D1,D2]) :- matrix_principalDiagonal(M, D1), matrix_verticalFlip(M, MF), matrix_principalDiagonal(MF, D2).

%:- matrix_diagonals([[1,2,3],[4,5,6],[7,8,9]], D).
%@ D = [[1, 5, 9], [3, 5, 7]].

% calls a number of times some predicate,
% it's good for defining cyclic not terminating predicates with periods that are known
% For instance, xmas(X) can be defined in a rather convulsed way to always output XMAS, SAMX, ... forever.
% But we know that there are only 2 distinct solutions.
call_times(Goal0, N) :- between(1, N, I), call_nth(Goal0, I).

% WARNING: this predicate is not complete! only works for true relations, and nonterminate otherwise!
squareSubmatrixAlongX_ofSize_forMatrix(M, N, M) :- width_matrix(N, M), !.
squareSubmatrixAlongX_ofSize_forMatrix(S, N, M) :-
    n_firstColumns_ofMatrix_rightMatrix(N, St, M, _), transpose(St, S).
squareSubmatrixAlongX_ofSize_forMatrix(S, N, M) :-
    firstColumn_ofMatrix_rightMatrix(_, M, RM),
    squareSubmatrixAlongX_ofSize_forMatrix(S, N, RM).

%:- squareSubmatrixAlongX_ofSize_forMatrix(S, 2, [[1,2,3,4],[5,6,7,8]]).
%@ S = [[1, 2], [5, 6]] ;
%@ S = [[2, 3], [6, 7]] ;
%@ S = [[3, 4], [7, 8]].

%:-squareSubmatrixAlongX_ofSize_forMatrix(S, 3,
%    [[1,2,4,8,16],
%     [3,6,12,24,48],
%     [1,2,4,8,16]]).
%@ S = [[1, 2, 4], [3, 6, 12], [1, 2, 4]] ;
%@ S = [[2, 4, 8], [6, 12, 24], [2, 4, 8]] ;
%@ S = [[4, 8, 16], [12, 24, 48], [4, 8, 16]].

% This one will let's us move a kernel throught all the matrix
squareSubmatrix_ofSize_forMatrix(S, N, S) :- size_matrix(N-N, S), !.
squareSubmatrix_ofSize_forMatrix(H, N, M) :- n_firstRows_ofMatrix(N, Fr, M),
                                             squareSubmatrixAlongX_ofSize_forMatrix(H, N, Fr).
squareSubmatrix_ofSize_forMatrix(H, N, [_|M]) :- squareSubmatrix_ofSize_forMatrix(H, N, M).

%:- squareSubmatrix_ofSize_forMatrix(Sm, 2,
%       [[1,2,3],
%        [4,5,6],
%        [6,7,8]]
%   ).
%@ Sm = [[1, 2], [4, 5]] ;
%@ Sm = [[2, 3], [5, 6]] ;
%@ Sm = [[4, 5], [6, 7]] ;
%@ Sm = [[5, 6], [7, 8]] ;
%@ false.

%:-squareSubmatrix_ofSize_forMatrix(S, 3,
%    [[1,2,4,8,16],
%     [3,6,12,24,48],
%     [1,2,4,8,16],
%     [3,6,12,24,48]]).
%@ S = [[1, 2, 4], [3, 6, 12], [1, 2, 4]] ;
%@ S = [[2, 4, 8], [6, 12, 24], [2, 4, 8]] ;
%@ S = [[4, 8, 16], [12, 24, 48], [4, 8, 16]] ;
%@ S = [[3, 6, 12], [1, 2, 4], [3, 6, 12]] ;
%@ S = [[6, 12, 24], [2, 4, 8], [6, 12, 24]] ;
%@ S = [[12, 24, 48], [4, 8, 16], [12, 24, 48]] ;
%@ false.

%:-squareSubmatrix_ofSize_forMatrix(S, 3,
%                                 [`XMASX`,
%                                  `XMASA`,
%                                  `AMAMA`,
%                                  `SAMXS`]).
%@ S = [`XMA`, `XMA`, `AMA`] ;
%@ S = [`MAS`, `MAS`, `MAM`] ;
%@ S = [`ASX`, `ASA`, `AMA`] ;
%@ S = [`XMA`, `AMA`, `SAM`] ;
%@ S = [`MAS`, `MAM`, `AMX`] ;
%@ S = [`ASA`, `AMA`, `MXS`] ;
%@ false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                %%%
%%%           XMAS detector and Kernel (4x4 matrix masks)          %%%
%%%                                                                %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% kernels will be of some character to be match against or nothing, so here is a:
listOfNothing([]).
listOfNothing([nothing|X]) :- listOfNothing(X).

% obviusly you would want to do this instead:
% xmas(`XMAS`)
% xmas(`SAMX`)
% but this is more fun:
xmas_(`XMAS`).
xmas_(X) :- xmas_(Y), reverse(Y,X).
xmas(X) :- call_times(xmas_(X), 2).
% and I hope that I convinced you that call_times it's a pretty
% nice predicate, we'll be using it a little more.

% the kernels:
%
% XMAS
% XMAS
% XMAS
% XMAS
% and its reverse permutation:
horizontalFullKernel([XMas,XMas,XMas,XMas]) :- xmas(XMas).

% ....
% ....
% ....
% XMAS
% and its permutation:
lastRowKernel([FourNothing, FourNothing, FourNothing, Xmas]) :-
    length(FourNothing, 4),
    listOfNothing(FourNothing),
    xmas(Xmas).

% ...X
% ...M
% ...A
% ...S
lastColumnKernel(K) :- lastRowKernel(Kh), transpose(Kh, K).


% XXXX
% MMMM
% AAAA
% SSSS
% and its reverse permutation:
verticalFullKernel(K) :- horizontalFullKernel(Kh), transpose(Kh,K).

%:- verticalFullKernel(K).
%@ K = [`XXXX`, `MMMM`, `AAAA`, `SSSS`] ;
%@ K = [`SSSS`, `AAAA`, `MMMM`, `XXXX`] ;
%@ false.


% X... % ...S
% .M.. % ..A.
% ..A. % .M..
% ...S % X...
% and its permutations
diagonalKernel(K) :- call_times(diagonalKernel_(K), 4).
diagonalKernel_([[0'X,N,N,N],[N,0'M,N,N],[N,N,0'A,N],[N,N,N,0'S]]) :- N = nothing.
diagonalKernel_([[N,N,N,0'X],[N,N,0'M,N],[N,0'A,N,N],[0'S,N,N,N]]) :- N = nothing.
diagonalKernel_(K) :- diagonalKernel_(R), reverse(R, K).

%:- diagonalKernel(K).
%@ K = [[88, nothing, nothing, nothing], [nothing, 77, nothing, nothing], [nothing, nothing, 65, nothing], [nothing, nothing, nothing, 83]] ;
%@ K = [[nothing, nothing, nothing, 88], [nothing, nothing, 77, nothing], [nothing, 65, nothing, nothing], [83, nothing, nothing, nothing]] ;
%@ K = [[nothing, nothing, nothing, 83], [nothing, nothing, 65, nothing], [nothing, 77, nothing, nothing], [88, nothing, nothing, nothing]] ;
%@ K = [[83, nothing, nothing, nothing], [nothing, 65, nothing, nothing], [nothing, nothing, 77, nothing], [nothing, nothing, nothing, 88]] ;
%@ false.

% so now we have a set of kernels that can be matched against the
% matrix of characters. These are the predicates to do the masking:

kernelmask_vector_booleans([], [], []).
kernelmask_vector_booleans([nothing|K], [_|M], [0|B]) :- kernelmask_vector_booleans(K,M,B).
kernelmask_vector_booleans([X|K], [X|V], [1|B]) :- dif(nothing,X), kernelmask_vector_booleans(K,V,B).
kernelmask_vector_booleans([X|K], [Y|V], [0|B]) :- dif(X,Y), dif(X, nothing), kernelmask_vector_booleans(K,V,B).

kernelmask_matrix_booleans([], [], []).
kernelmask_matrix_booleans([Krow|K], [Mrow|M], [Brow|B]) :-
    kernelmask_vector_booleans(Krow, Mrow, Brow), kernelmask_matrix_booleans(K, M, B).

% so for instance:
%:- horizontalFullKernel(K),
%   kernelmask_matrix_booleans(K,
%       [`XMAS`
%       ,`XMMS`
%       ,`SAMX`
%       ,`MASX`],
%       Bs).
%@ K = [`XMAS`, `XMAS`, `XMAS`, `XMAS`],
%@ Bs = [[1, 1, 1, 1], [1, 1, 0, 1], [0, 0, 0, 0], [0, 0, 0, 0]] ;
%@ K = [`SAMX`, `SAMX`, `SAMX`, `SAMX`],
%@ Bs = [[0, 0, 0, 0], [0, 0, 1, 0], [1, 1, 1, 1], [0, 1, 0, 1]] ;
%@ false.

% so you can see that counting full rows of 1s you have the ocurrences of 'xmas'
% for diagonals its a little more complicated, we have to extract the diagonals to check
%:- diagonalKernel(K),
%   kernelmask_matrix_booleans(
%       K,
%       [`X...`,
%        `.M..`,
%        `..A.`,
%        `...S`],
%       Bs),
%   matrix_diagonals(Bs, D).
%@ K = [[88, nothing, nothing, nothing], [nothing, 77, nothing, nothing], [nothing, nothing, 65, nothing], [nothing, nothing, nothing, 83]],
%@ Bs = [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]],
%@ D = [[1, 1, 1, 1], [0, 0, 0, 0]] ;
%@ K = [[nothing, nothing, nothing, 88], [nothing, nothing, 77, nothing], [nothing, 65, nothing, nothing], [83, nothing, nothing, nothing]],
%@ Bs = [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]],
%@ D = [[0, 0, 0, 0], [0, 0, 0, 0]] ;
%@ K = [[nothing, nothing, nothing, 83], [nothing, nothing, 65, nothing], [nothing, 77, nothing, nothing], [88, nothing, nothing, nothing]],
%@ Bs = [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]],
%@ D = [[0, 0, 0, 0], [0, 0, 0, 0]] ;
%@ K = [[83, nothing, nothing, nothing], [nothing, 65, nothing, nothing], [nothing, nothing, 77, nothing], [nothing, nothing, nothing, 88]],
%@ Bs = [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]],
%@ D = [[0, 0, 0, 0], [0, 0, 0, 0]] ;
%@ false.

% as you can see, only the first diagonal of the first result has only 1s, and thats is what we want.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                %%%
%%%                     Finally, XMAS counting                     %%%
%%%                                                                %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% these family of functions are crying to be refactored ...

% Given a 4x4 matrix, let's count in different ways:
matrix4x4_fullHorizontalXmasCount(M,C) :- findall(Ck, matrix4x4_fullHorizontalXmasCount_(M,Ck), L), sum_list(L, C).
matrix4x4_fullHorizontalXmasCount_(M, C) :-
    horizontalFullKernel(K), kernelmask_matrix_booleans(K, M, Bs),
    findall(1, member([1,1,1,1], Bs), L), sum_list(L, C).

%:- matrix4x4_fullHorizontalXmasCount(
%       [`XMAS`,
%        `....`,
%        `....`,
%        `SAMX`],
%       C
%   ).
%@ C = 2.

matrix4x4_fullVerticalXmasCount(M,C) :- findall(Ck, matrix4x4_fullVerticalXmasCount_(M, Ck), L), sum_list(L, C).
matrix4x4_fullVerticalXmasCount_(M,C) :-
    verticalFullKernel(K), kernelmask_matrix_booleans(K, M, Bs0), transpose(Bs0, Bs),
    findall(1, member([1,1,1,1], Bs), L), sum_list(L, C).

%:- matrix4x4_fullVerticalXmasCount(
%       [`XMAS`,
%        `M..A`,
%        `A..M`,
%        `S..X`],
%       C
%   ).
%@ C = 2.

matrix4x4_diagonalXmasCount(M, C) :- findall(Ck, matrix4x4_diagonalXmasCount_(M, Ck), L), sum_list(L, C).
matrix4x4_diagonalXmasCount_(M, C) :-
    diagonalKernel(K), kernelmask_matrix_booleans(K, M, Bs), matrix_diagonals(Bs, Diags),
    findall(1, member([1,1,1,1], Diags), L), sum_list(L, C).

%:- matrix4x4_diagonalXmasCount(
%       [`X..S`,
%        `.MA.`,
%        `.MA.`,
%        `X..S`],
%       C
%   ).
%@ C = 2.


matrix4x4_lastRowXMasCount(M, C) :- findall(Ck, matrix4x4_lastRowXMasCount_(M, Ck), L), sum_list(L, C).
matrix4x4_lastRowXMasCount_(M, C) :-
    lastRowKernel(K),  kernelmask_matrix_booleans(K, M, Bs),
    findall(1, member([1,1,1,1], Bs), L), sum_list(L, C).

%:- matrix4x4_lastRowXMasCount(
%       [`XMAS`,
%        `XMAS`,
%        `XMAS`,
%        `SAMX`],
%       C
%   ).
%@ C = 1.

matrix4x4_lastColumnXmasCount(M, C) :- findall(Ck, matrix4x4_lastColumnXmasCount_(M, Ck), L), sum_list(L, C).
matrix4x4_lastColumnXmasCount_(M, C) :-
    lastColumnKernel(K),  kernelmask_matrix_booleans(K, M, BsT), transpose(BsT, Bs),
    findall(1, member([1,1,1,1], Bs), L), sum_list(L, C).

%:- matrix4x4_lastColumnXmasCount(
%       [`XMAS`,
%        `MMAA`,
%        `AMAM`,
%        `SAMX`],
%       C
%   ).
%@ C = 1.


% Why only the last one? because we'll moving the kernel one cell at
% the time, so only the last row won't be already calculated

% but, we'll move horizontal first, so only last vertical column won't be already calculated
% then, when we move down, only last row won't be calculated ...

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% So we have 4 cases:                                                                                                                 %
% 1- first topleft submatrix: count with all the kernels                                                                              %
% 2- we're moving to the right in the first 4 rows, *do not* count all vertical, count only last one to avoid counting more than once %
% 3- we just moved one row below: *do not* count all horizontal, count only last one to avoid counting more than once                 %
% 4- we're moving to the right and there are rows above: similar to 4, but with 3, only last row horizontal is needed                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% So for the first 4 rows of the matrix, we can count all the horizontal, the last vertical and diagonals
% we're in case 1
% 1- first topleft submatrix: count with all the kernels
case1_xmasCount(M, C) :-
    matrix4x4_fullHorizontalXmasCount(M, Ch),
    matrix4x4_diagonalXmasCount(M, Cd),
    matrix4x4_fullVerticalXmasCount(M, Cv),
    C #= Ch + Cd + Cv.

% 2- we're moving to the right in the first 4 rows, *do not* count all vertical, count only last one to avoid counting more than once
case2_xmasCount(M, C) :- findall(Ci, case2_xmasCount_(M, Ci), Cs), sum_list(Cs, C).
case2_xmasCount_(M, C) :-
    n_firstRows_ofMatrix(4, Ms, M),
    squareSubmatrixAlongX_ofSize_forMatrix(S, 4, Ms),
    matrix4x4_fullHorizontalXmasCount(S, Ch),
    matrix4x4_diagonalXmasCount(S, Cd),
    matrix4x4_lastColumnXmasCount(S, Cv),
    C #= Ch + Cd + Cv.

% :- case2_xmasCount_(
%     [`XMASXMAS`,
%      `.M......`,
%      `..A.....`,
%      `...S....`], C).
%@ C = 2 ;
%@ C = 0 ;
%@ C = 0 ;
%@ C = 0 ;
%@ C = 1 ;
%@ false.

% :- case2_xmasCount(
%     [`XMASXMAS`,
%      `.M......`,
%      `..A.....`,
%      `...S....`], C).
%@ C = 3.

% 3- we just moved one row below: *do not* count all horizontal, count only last one to avoid counting more than once
case3_xmasCount(M, C) :-
    splitMatrix_atColumn_leftMatrix_rightMatrix(M, 4, Cols, _),
    n_firstRows_ofMatrix(4, SM, Cols),
    matrix4x4_diagonalXmasCount(SM, Cd),
    matrix4x4_fullVerticalXmasCount(SM, Cv),
    matrix4x4_lastRowXMasCount(SM, Ch),
    C #= Ch + Cd + Cv.

% :- case3_xmasCount(
%     [`XMASXMAS`,
%      `XMASM...`,
%      `XMASA...`,
%      `XMASS...`], C).
%@ C = 3;
%@ false.

% 4- we're moving to the right and there are rows above: similar to 4, but with 3, only last row horizontal is needed
case4_xmasCount(M, C) :- findall(Ci, case4_xmasCount_(M, Ci), Cs), sum_list(Cs, C).
case4_xmasCount_(M, C) :-
    n_firstRows_ofMatrix(4, Ms, M),
    squareSubmatrixAlongX_ofSize_forMatrix(S, 4, Ms),
    matrix4x4_lastColumnXmasCount(S, Cv),
    matrix4x4_lastRowXMasCount(S, Ch),
    matrix4x4_diagonalXmasCount(S, Cd),
    C #= Ch + Cd + Cv.

% :- case4_xmasCount(
%     [`XMASXMAS`,
%      `XMASM...`,
%      `XMASA...`,
%      `XMASS...`], C).
%@ C = 4.

case3and4_xmasCount(M, C) :-
    height_matrix(4, M),
    case3_xmasCount(M, C3),
    firstColumn_ofMatrix_rightMatrix(_, M, RM),
    case4_xmasCount(RM, C4),
    C #= C3 + C4.
case3and4_xmasCount(M, C) :-
    height_matrix(H, M), H #> 4,
    n_firstRows_ofMatrix(4, FR, M),
    case3and4_xmasCount(FR, C1),
    firstRow_bottomMatrix_ofMatrix(_, BM, M),
    case3and4_xmasCount(BM, CR),
    C #= C1 + CR.

% finally we have to combine all cases
matrix_xmasCount(M, C) :-
    splitMatrix_atColumn_leftMatrix_rightMatrix(M, 4, Cols, _),
    n_firstRows_ofMatrix(4, TopLeftMatrix, Cols),
    case1_xmasCount(TopLeftMatrix, C1),
    splitMatrix_atColumn_leftMatrix_rightMatrix(M, 1, _, TopRightMatrix),
    case2_xmasCount(TopRightMatrix, C2),
    firstRow_bottomMatrix_ofMatrix(_, BM, M),
    case3and4_xmasCount(BM, C3_4),
    C #= C1 + C2 + C3_4.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                %%%
%%%   parsing of character codes to a matrix of character codes    %%%
%%%                                                                %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

spaces --> ` ` | ` `, spaces.
maybe_spaces --> `` | spaces.
maybe_newline --> `` | `\n`.
not_space(X) --> {dif(` `, [X])}.
not_newline(X) --> {dif(`\n`, [X])}.
rowOfChars(R) --> maybe_spaces, rowOfChars_(R), maybe_spaces.
rowOfChars_([]) --> ``.
rowOfChars_([X|Xs]) --> [X], not_space(X), not_newline(X), rowOfChars_(Xs).
matrixOfChars([M]) --> rowOfChars(M), maybe_spaces, maybe_newline.
matrixOfChars([L|M]) --> rowOfChars(L), `\n`, matrixOfChars(M).

:- phrase(rowOfChars(_), `MMMSXXMASM`).
:- phrase(rowOfChars(_), `     MMMSXXMASM`).
:- phrase(rowOfChars(_), `     MMMSXXMASM     `).
:- \+ phrase(rowOfChars(_),
          `MMMSXXMASM
MSAMXMSMSA`).
:- phrase(matrixOfChars(_),
          `MMMSXXMASM
           MSAMXMSMSA
          `).
:- phrase(matrixOfChars(_),
          `MMMSXXMASM
           MSAMXMSMSA
           AMXSXMAAMM
           MSAMASMSMX
           XMASAMXAMM
           XXAMMXXAMA
           SMSMSASXSS
           SAXAMASAAA
           MAMMMXMMMM
           MXMXAXMASX`).

:- phrase(matrixOfChars(M),
          `MMMSXXMASM
          MSAMXMSMSA
          AMXSXMAAMM
          MSAMASMSMX
          XMASAMXAMM
          XXAMMXXAMA
          SMSMSASXSS
          SAXAMASAAA
          MAMMMXMMMM
          MXMXAXMASX`),
   matrix_xmasCount(M, C), C=18.

%% --- Part Two ---

%% The Elf looks quizzically at you. Did you misunderstand the assignment?

%%                                   Looking for the instructions, you flip over the word search to find that this isn't actually an XMAS puzzle; it's an X-MAS puzzle in which you're supposed to find two MAS in the shape of an X. One way to achieve that is like this:

% M.S
% .A.
% M.S

%% Irrelevant characters have again been replaced with . in the above diagram. Within the X, each MAS can be written forwards or backwards.

%% Here's the same example from before, but this time all of the X-MASes have been kept instead:

%% .M.S......
%% ..A..MSMS.
%% .M.S.MAA..
%% ..A.ASMSM.
%% .M.S.M....
%% ..........
%% S.S.S.S.S.
%% .A.A.A.A..
%% M.M.M.M.M.
%% ..........

%% In this example, an X-MAS appears 9 times.

%% Flip the word search from the instructions back over to the word search side and try again. How many times does an X-MAS appear?

% Okey ... but this is so much easier!
mas(`MAS`).
mas(`SAM`).

part2Kernel(
    [[X0,      nothing, X1     ],
     [nothing, X2,      nothing],
     [X3,      nothing, X4     ]]
) :- mas([X0,X2,X4]), mas([X1,_,X3]).

% :- part2Kernel(K).
%@ K = [[77, nothing, 77], [nothing, 65, nothing], [83, nothing, 83]] ;
%@ K = [[77, nothing, 83], [nothing, 65, nothing], [77, nothing, 83]] ;
%@ K = [[83, nothing, 77], [nothing, 65, nothing], [83, nothing, 77]] ;
%@ K = [[83, nothing, 83], [nothing, 65, nothing], [77, nothing, 77]].

matrix_masCount(M, C) :- findall(Ci, matrix_masCount_(M, Ci), Cs), sum_list(Cs, C).
matrix_masCount_(M, C) :-
    squareSubmatrix_ofSize_forMatrix(S, 3, M),
    part2Kernel(K),
    kernelmask_matrix_booleans(K, S, B),
    matrix_diagonals(B, D),
    (   D=[[1,1,1],[1,1,1]] -> C=1 ; C=0   ).

:- matrix_masCount(
      [`.M.S......`,
       `..A..MSMS.`,
       `.M.S.MAA..`,
       `..A.ASMSM.`,
       `.M.S.M....`,
       `..........`,
       `S.S.S.S.S.`,
       `.A.A.A.A..`,
       `M.M.M.M.M.`,
       `..........`],
      C), C=9.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                %%%
%%%                            MAIN / IO                           %%%
%%%                                                                %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- initialization(main, main).
main([File]) :-
    phrase_from_file(matrixOfChars(M), File),
    size_matrix(H-W, M), S #= H*W,
    matrix_xmasCount(M, C1),
    matrix_masCount(M, C2),
    writef("archivo: %w con %w caracteres, %w xmas y %w x-mas",
           [File, S, C1, C2]).


