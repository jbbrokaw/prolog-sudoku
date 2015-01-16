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


