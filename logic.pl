ensure_loaded(sudoku).
ensure_loaded(utility).

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


/* possibilities/4 finds the possible values for
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


/* The "obvious" fill-ins are the ones with only one possibility */

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


/* Need a way to fill in obvious slots until there are none left:
first need to count unknown items */


numUnknownRow([], 0).
numUnknownRow([X|Rest], N):-
	var(X),
	numUnknownRow(Rest, N2),
	N is N2 + 1, !.
numUnknownRow([X|Rest], N):-
	nonvar(X),
	numUnknownRow(Rest, N).

numUnknown([], 0).
numUnknown([Row|RestOfBoard], N):-
	numUnknownRow(Row, NRow),
	numUnknown(RestOfBoard, NRest),
	N is NRow + NRest, !.

/* Now fill obvious until numUnknown doesn't change */
cycledObvious(_, 0).
cycledObvious(Board, N):-
	N > 0,
	numUnknown(Board, NumOrig),
	filledObvious(Board),
	numUnknown(Board, NumNew),
	N2 is NumOrig - NumNew,
	cycledObvious(Board, N2), !.


cycledObvious(Board):-
	cycledObvious(Board, 1).


/* Now we need to find less obvious stuff, like if there is
only one possible place to put a number in a row/column/block
call these "constraints"
For this, I will just build a holder of all possibilities once,
then consult it as we cycle over rows, columns, and blocks
rather than determining possibilities for each item */
rowPossibilities(_, 10, _, []).

rowPossibilities(RowNum, ColNum, Board, [Possibilities|OtherPossibilities]):-
	C2 is ColNum + 1,
	rowPossibilities(RowNum, C2, Board, OtherPossibilities),
	possibilities(Board, RowNum, ColNum, Possibilities).

buildPossibilities(10, _, []).
buildPossibilities(RowNum, Board, [RowPossibilities|RestOfPossibilities]):-
	R2 is RowNum + 1,
	buildPossibilities(R2, Board, RestOfPossibilities),
	rowPossibilities(RowNum, 1, Board, RowPossibilities),
	length(RowPossibilities, 9), !.

buildPossibilities(Board, PossArray):-
	buildPossibilities(1, Board, PossArray).

/* Need to find the potential positions for X from a list of possibilities */
potentialPositions(_, [], []):- !.
potentialPositions(X, [Possibilities|OtherPossibilities], [Position|P2]):-
	member(X, Possibilities),
	length(OtherPossibilities, N),
	Position is 9 - N,
	potentialPositions(X, OtherPossibilities, P2), !.
potentialPositions(X, [_|OtherPossibilities], Positions):-
	potentialPositions(X, OtherPossibilities, Positions), !. /*Not a member */

/* An "Element" is a row, column, or block (as a list of numbers); these cycle through needed numbers
for each element looking for constraints to enforce*/
enforcedConstraints([], _, _, _).
enforcedConstraints([NumberToPlace|Rest], PossArray, ElementNum, Element):-
	potentialPositions(NumberToPlace, PossArray, Positions),
	length(Positions, 1),
	nth0(0, Positions, Position),
	P0 is Position - 1,
	nth0(P0, Element, NumberToPlace),
	enforcedConstraints(Rest, PossArray, ElementNum, Element).
enforcedConstraints([_|Rest], PossArray, ElementNum, Element):-
	enforcedConstraints(Rest, PossArray, ElementNum, Element).

/* Now cycle through rows, columns, and blocks enforcing these "constraints" */
constrainedRows(10, _, _).
constrainedRows(RowNum, Board, PossArray):-
	row(RowNum, Board, Row),
	complement(Row, Needed),
	row(RowNum, PossArray, RowPossibilities),
	enforcedConstraints(Needed, RowPossibilities, RowNum, Row),
	R2 is RowNum + 1,
	constrainedRows(R2, Board, PossArray).

constrainedRows(Board):-
	buildPossibilities(Board, PossArray),
	constrainedRows(1, Board, PossArray).

constrainedColumns(10, _, _).
constrainedColumns(ColNum, Board, PossArray):-
	column(ColNum, Board, Column),
	complement(Column, Needed),
	column(ColNum, PossArray, ColPossibilities),
	enforcedConstraints(Needed, ColPossibilities, ColNum, Column),
	C2 is ColNum + 1,
	constrainedColumns(C2, Board, PossArray).

constrainedColumns(Board):-
	buildPossibilities(Board, PossArray),
	constrainedColumns(1, Board, PossArray).


constrainedBlocks(10, _, _).
constrainedBlocks(BlockNum, Board, PossArray):-
	block(BlockNum, Board, Block),
	complement(Block, Needed),
	block(BlockNum, PossArray, BlockPossibilities),
	enforcedConstraints(Needed, BlockPossibilities, BlockNum, Block),
	B2 is BlockNum + 1,
	constrainedBlocks(B2, Board, PossArray).

constrainedBlocks(Board):-
	buildPossibilities(Board, PossArray),
	constrainedBlocks(1, Board, PossArray).


/* That should more or less be the tools we need, here is a container to do all the logic */

cycledLogic(Board, _):-
	numUnknown(Board, 0), !,
	writef("Completed\n", []),
	pp(Board).

cycledLogic(Board, 0):-
	writef("We are stuck.\n", []),
	numUnknown(Board, Unk),
	writef("Number of unknowns: %w\n", [Unk]),
	buildPossibilities(Board, PossArray),
	pp(Board),
	pp(PossArray).

cycledLogic(Board, N):-
	numUnknown(Board, NumOrig),
	writef("Beginning round, Unknowns: %w\n", [NumOrig]),
	pp(Board),
	N > 0,
	cycledObvious(Board, 1), !,
	constrainedRows(Board), !,
	constrainedColumns(Board), !,
	constrainedBlocks(Board), !,
	numUnknown(Board, NumNew),
	Change is NumOrig - NumNew,
	cycledLogic(Board, Change).

cycledLogic(Board):- cycledLogic(Board, 1).

/* What to do if we're stuck? Find an option & guess, but go logically from there
(Probably solved(Board) is still too inefficient) */











