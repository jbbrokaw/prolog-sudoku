load_files(sudoku).

/* For 'complement(Known, Possible)', known is
a list of length 9, where some terms may be variables.
Possiblities is the possibilities for each variable.
*/
complement([], [1,2,3,4,5,6,7,8,9]).
complement([X|Rest], Possibilities):-
	complement(Rest, Others),
	nonvar(X),
	select(X, Others, Possibilities).
complement([X|Rest], Possibilities):-
	complement(Rest, Others),
	var(X),
	Possibilities = Others.


/* Possibilities/4 finds the possible values for
the number at (RowNum, ColumnNum) of board.
*/

blockNum(N, RowNum, ColumnNum):-
	N is  3 * (div(RowNum - 1, 3)) + div(ColumnNum - 1, 3) + 1.

possibilities(Board, RowNum, ColumnNum, Possibilities):-
	row(RowNum, Board, Row),
	row(ColumnNum, Row, Value),
	nonvar(Value),
	append([Value],[],Possibilities), !.

possibilities(Board, RowNum, ColumnNum, Possibilities):-
	row(RowNum, Board, Row),
	complement(Row, RowPossibilities),
	column(ColumnNum, Board, Column),
	complement(Column, ColumnPossibilities),
	blockNum(BlockNum, RowNum, ColumnNum),
	block(BlockNum, Board, Block),
	complement(Block, BlockPossibilities),
	intersection(RowPossibilities, ColumnPossibilities, RCPoss),
	intersection(RCPoss, BlockPossibilities, Possibilities), !.


/* The "obvious" fill-ins are the ones with only one possibility, do those
below */

filledObviousRows(10, _).
filledObviousRows(RowNum, Board):-
	filledObviousColumns(RowNum, 1, Board),
	N is RowNum + 1,
	filledObviousRows(N, Board).

filledObviousColumns(_, 10, _).
filledObviousColumns(RowNum, ColNum, Board):-
	row(RowNum, Board, Row),
	row(ColNum, Row, Value),
	nonvar(Value),
	N is ColNum + 1,
	filledObviousColumns(RowNum, N, Board).

filledObviousColumns(RowNum, ColNum, Board):-
	possibilities(Board, RowNum, ColNum, Possibilities),
	length(Possibilities, X),
	X > 1,
	N is ColNum + 1,
	filledObviousColumns(RowNum, N, Board).

filledObviousColumns(RowNum, ColNum, Board):-
	possibilities(Board, RowNum, ColNum, Possibilities),
	length(Possibilities, 1),
	row(RowNum, Board, Row),
	row(ColNum, Row, Value),
	member(Value, Possibilities),
	N is ColNum + 1,
	filledObviousColumns(RowNum, N, Board).

filledObvious(Board):-
	filledObviousRows(1, Board).








