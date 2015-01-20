# prolog-suduoku

## A short solver of sudoku boards

Input a board like this:

```prolog
unsolvedBoard(X):-
	X = [[5, 3, _,   _, 7, _,   _, _, _],
	     [6, _, _,   1, 9, 5,   _, _, _],
	     [_, 9, 8,   _, _, _,   _, 6, _],

	     [8, _, _,   _, 6, _,   _, _, 3],
	     [4, _, _,   8, _, 3,   _, _, 1],
	     [7, _, _,   _, 2, _,   _, _, 6],

	     [_, 6, _,   _, _, _,   2, 8, _],
	     [_, _, _,   4, 1, 9,   _, _, 5],
	     [_, _, _,   _, 8, _,   _, 7, 9]].
```

Then, to solve

```prolog
unsolvedBoard(X), solved(X).
```

Its performance is incredibly bad, but it's well under 100 lines of code!

To be a little more logical and efficient, `logic.pl` has a lot of code solving it more like a human.
To solve in a reasonable amount of time:

```prolog
unsolvedBoard(X), logicWithGuesses(X).
```

MIT Licence.