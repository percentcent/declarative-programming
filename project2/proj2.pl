%% COMP90048 Declarative Programming
%% Authored by Peishan Li, student ID 905508

%% The purpose of this project is to solve specific math puzzles.
%% A maths puzzle is a square grid of squares, each to be filled in 
%% with a single digit 1–9 (zero is not permitted) satisfying these constraints:
%% each row and each column contains no repeated digits;
%% all squares on the diagonal line from upper left to lower right contain the same value;
%% the heading of reach row and column holds either the sum or the product 
%% of all the digits in that row or column.
%% The program is to be able 2x2,3x3,4x4 puzzles.

%% My approach is to check each constraint and ensure it is valid.
%% The basic idea consists of 3 steps,
%% first,get elements on the digonal line, ensure they are the same,
%% second, check each row to satisfy the constraints,
%% transpose the puzzle and check the columns as step 2.
%% In order to implement the checking function, I use some extra assistant predicates,
%% including sumList, productList, all_Same, etc.


:- ensure_loaded(library(clpfd)).

%% puzzle_solution/1
%% its argument will be a proper list of proper lists, 
%% and all the header squares of the puzzle are bound to integers.
%% first check the diagonal elements of the Puzzle
%% then use checkRows and checkColumns to fill the Puzzle according to the game constraints

puzzle_solution(Puzzle) :-
	getDiagonalElem(Puzzle,1,Result),
	all_same(Result),
	checkRows(Puzzle),
	checkColumns(Puzzle).

%% getDiagonalElem/3 is to get the diagonal elements of the Puzzle.
%% it use Prolog library modules nthO, 
%% True when Elem is the Index element of List. Counting starts at 0.

getDiagonalElem([L|LS],N,[R|RS]) :-
	nth0(1,[L|LS],L1),
	nth0(N,L1,R),
	N2 is N + 1,
	getDiagonalElem(LS,N2,RS),!.
getDiagonalElem(List,_,[]) :-
	length(List,1).

%% allSame/1 is to check whether the elements in a list are all the same.
%% it use another predicate listof.

all_same(List) :-
	listof(_, List).

listof(_, []).
listof(Elt, [Elt|List]) :-
	listof(Elt, List).

%% checkRows/1 is to conform all rows satisfying the constraints.
%% it use sumList, productList to confirm 
%% the heading of reach row and column holds either the sum or the product

checkRows([]).
checkRows([X|XS]) :-
	nth0(1,[X|XS],[Y|YS]),
	is_set(YS),
	(
		sumList(YS,0,Y);
		productList(YS,1,Y)
	),
	checkRows(XS).
checkRows(List) :-
	length(List,1),
	nth0(0,List,[H|HS]),
	(
		sumList(HS,0,H);
		productList(HS,1,H)
	).

%% checkColumns/1 is to conform all columns satisfying the constraints.
%% transpose the puzzle, so the columns in original puzzle now become rows,
%% then, use the predicate as used for checking rows.

checkColumns(List) :-
	transpose(List, TList),
	checkRows(TList).

%% sumList/2 is True when the head of a list equals to the sum of the rest of the list
%% use prolog built-in predicate between/3 to put domain limits
sumList([], Sum, Sum).
sumList([X|Xs], Sum0, Sum) :-
	between(1,9,X),
	Sum1 is Sum0 + X,
	sumList(Xs, Sum1, Sum).

%% productList/2 is True when the head of a list equals to the product of the rest of the list
%% use prolog built-in predicate between/3 to put domain limits
productList([],Product, Product).
productList([X|Xs], Product0, Product) :-
	between(1,9,X),
	Product1 is Product0 * X,
	productList(Xs, Product1, Product).