/*
File	   : numbers.pl
Author     : Ke Shi (MSC,Dept.CIS)
Stu ID     : 742396
Date       : Tue Oct 18 2015
Purpose    : Solve Number Puzzle problem
*/

/*
	the idea of solving this puzzle problem is,
	first, get elements on diagonal, and let them be same,
	second, get every row, let them conform the constraints 
	(the head is whether the sum or product of the rest elements),
	third, transpose the puzzle, so the columns in original puzzle now become rows,
	then, use the predicate as used for checking rows.
*/

:- ensure_loaded(library(clpfd)).

/*
	--check/1--
	True if Puzzle conforms the constraints 
	this predicate will check the rows and columns respectively
*/
check(Puzzle) :-
	check_rows(Puzzle),
	check_columns(Puzzle).

/*
	--check_rows/1--
	True if the rows comform to the constraints	
*/
check_rows([]).
check_rows([X|XS]) :-
	nth0(1,[X|XS],[Y|YS]),
	is_set(YS),
	(
		sumList(YS,Y)
		;
		productList(YS,Y)
	),
	check_rows(XS).

check_rows(List) :-
	length(List,1),
	nth0(0,List,[H|HS]),
	(
		sumList(HS,H)
		;
		productList(HS,H)
	).

/*
	--check_columns/1--
	True if the rows comform to the constraints	
*/
check_columns(List) :-
	transpose(List, TList),
	check_rows(TList).

/*
	--sumList/2--
	True when the head of a list 
	equals to the sum of the rest of the list 
*/
sumList(List, Sum) :-
	sumList(List, 0, Sum).

sumList([], Sum, Sum).
sumList([N|Ns], Sum0, Sum) :-
	between(1,9,N),
	Sum1 is Sum0 + N,
	sumList(Ns, Sum1, Sum).

/*
	--productList/2--
	True when the head of a list 
	equals to the product of the rest of the list 
*/
productList(List, Product) :-
	productList(List, 1, Product).

productList([],Product, Product).
productList([X|XS], Product0, Product) :-
	between(1,9,X),
	Product1 is Product0 * X,
	productList(XS, Product1, Product).

/*
	--getDiagonal/3--
	get the diagonal of the Puzzle
*/
getDiagonal([L|LS],N,[R|RS]) :-
	nth0(1,[L|LS],L1),
	nth0(N,L1,R),

	N2 is N + 1,
	getDiagonal(LS,N2,RS),!.

getDiagonal(List,_,[]) :-
	length(List,1).

/*
	--sameElem/1--
	True if all the elements in a list are same
*/
all_same(List) :-
	listof(_, List).

listof(_, []).
listof(Elt, [Elt|List]) :-
	listof(Elt, List).

/*
	--puzzle_solution/1--
	first get the diagonal elements of the Puzzle
	then let them be same
	then use check/1 to fill the Puzzle according to the game constraints 
*/
puzzle_solution(Puzzle) :-
	getDiagonal(Puzzle,1,Result),
	all_same(Result),
	check(Puzzle).