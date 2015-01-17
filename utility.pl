ensure_loaded(sudoku).

ppValue(10, _).
ppValue(N, Row):-
	row(N, Row, Val),
	nonvar(Val),
	N > 1,
	mod(N-1, 3) =:= 0,
	writef('\t%w ', [Val]),
	N2 is N + 1,
	ppValue(N2, Row).
ppValue(N, Row):-
	row(N, Row, Val),
	var(Val),
	N > 1,
	mod(N-1, 3) =:= 0,
	writef('\t%w ', ['_']),
	N2 is N + 1,
	ppValue(N2, Row).
ppValue(N, Row):-
	row(N, Row, Val),
	nonvar(Val),
	writef('%w ', [Val]),
	N2 is N + 1,
	ppValue(N2, Row).
ppValue(N, Row):-
	row(N, Row, Val),
	var(Val),
	writef('%w ', ['_']),
	N2 is N + 1,
	ppValue(N2, Row).

ppRow(10, _).
ppRow(N, Board):-
	N > 1,
	mod(N-1, 3) =:= 0,
	writef('\n', []),
	row(N, Board, Row),
	ppValue(1, Row),
	writef('\n', []),
	N2 is N + 1,
	ppRow(N2, Board).

ppRow(N, Board):-
	row(N, Board, Row),
	ppValue(1, Row),
	writef('\n', []),
	N2 is N + 1,
	ppRow(N2, Board).

pp(Board):- ppRow(1, Board), !, writef('\n\n', []).

