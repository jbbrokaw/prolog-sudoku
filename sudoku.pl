lpresent([],[]).
lpresent([Digit|Others], List):- member(Digit, List),
	select(Digit, List, NewList),
	lpresent(Others, NewList).


valid(List):-
	length(List,9),
	lpresent([1,2,3,4,5,6,7,8,9], List).

/*
board is [[1,2,3,4,5,6,7,8,9], [4,5,6,7,8,9,1,2,3], ...]
row(1, board, X) if X is the first row
column(1, board, X) if X is the first column
box(1, board, X) if X is the list of items in the first box
boxes go 1, 2 ,3
	 4, 5, 6
	 7, 8, 9
order matters for all these facts
*/

row(1, [Row|_], X):- X = Row.
row(N, [_|Rest], X):-
	NewN is N - 1,
	row(NewN, Rest, X).
/* Note that row(N, List, X) unifies X to the Nth item of List in general */

column(_, [], []).
column(N, [Row|OtherRows], [Digit|OtherDigits]):-
	row(N, Row, Digit),
	column(N, OtherRows, OtherDigits).


buildBlock(0, _, _, _, _).
/* I do the 3 values for each row randomly (No loop) but loop thru the 3 rows */
buildBlock(N, Row, Column, Board, Digits):-
	row(Row, Board, CurrentRow),
	D is 3 * (3 - N) + 1,
	row(Column, CurrentRow, X),
	row(D, Digits, X),
	C2 is Column + 1,
	D2 is D + 1,
	row(C2, CurrentRow, Y),
	row(D2, Digits, Y),
	C3 is Column + 2,
	D3 is D + 2,
	row(C3, CurrentRow, Z),
	row(D3, Digits, Z),
	N2 is N -1,
	R2 is Row + 1,
	buildBlock(N2, R2, Column, Board, Digits),
	length(Digits,9).

block(N, Board, Digits):-
     FirstCol is 3 * mod(N-1,3) + 1,
     FirstRow is 3 * div(N-1,3) + 1,
     buildBlock(3, FirstRow, FirstCol, Board, Digits).

rowsValid(0,_).
rowsValid(N, Board):-
	row(N,Board,Y),
	valid(Y),
	N2 is N - 1,
	rowsValid(N2, Board).

columnsValid(0,_).
columnsValid(N, Board):-
	column(N, Board, Y),
	valid(Y),
	N2 is N - 1,
	columnsValid(N2, Board).

blocksValid(0,_).
blocksValid(N, Board):-
	block(N, Board, Y),
	valid(Y),
	N2 is N - 1,
	blocksValid(N2, Board).

solved(Board):-
	rowsValid(9, Board),
	columnsValid(9, Board),
	blocksValid(9, Board).
