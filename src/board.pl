:- use_module(library(lists)).
:- use_module(library(random)).

% dynamic player(+Player,-PlayerType)
:- dynamic player/2.

% dynamic difficulty(+Computer,-Level)
:- dynamic difficulty_level/2.

% dynamic player_checker(+Player,-Symbol)
:- dynamic player_checker/2.

% dynamic neutral_pawn_coordinates(Row-Col)
:- dynamic neutral_pawn_coordinates/1.

% dynamic player_score(+Player, -Score)
:- dynamic player_score/2.

% board(+SizeOfBase,+Matrix)
% Board structure
% Board Small
board(13, [
    [empty, empty, empty, empty, empty, empty, notused, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, notused, notused, notused, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, notused, notused, notused, notused, notused, empty, empty, empty, empty],
    [empty, empty, empty, notused, notused, notused, notused, notused, notused, notused, empty, empty, empty],
    [empty, empty, notused, notused, notused, notused, notused, notused, notused, notused, notused, empty, empty],
    [empty, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, empty],
    [notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused]
]).

% Board Medium
board(17, [
    [empty, empty, empty, empty, empty, empty, empty, empty, notused, empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, notused, notused, notused, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, notused, notused, notused, notused, notused, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, notused, notused, notused, notused, notused, notused, notused, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, notused, notused, notused, notused, notused, notused, notused, notused, notused, empty, empty, empty, empty],
    [empty, empty, empty, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, empty, empty, empty],
    [empty, empty, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, empty, empty],
    [empty, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, empty],
    [notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused]
]).

% Board Large
board(21, [
    [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, notused, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty, empty, notused, notused, notused, empty, empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty, notused, notused, notused, notused, notused, empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, notused, notused, notused, notused, notused, notused, notused, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, notused, notused, notused, notused, notused, notused, notused, notused, notused, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, empty, empty, empty, empty],
    [empty, empty, empty, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, empty, empty, empty],
    [empty, empty, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, empty, empty],
    [empty, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, empty],
    [notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused]
]).

% board(+Cells, +Columns, +Rows)
board(13,5,3).
board(17,7,4).
board(21,9,5).

% other_player(+CurrentPlayer,-NextPlayer)
% Change player turn
other_player(player1, player2).
other_player(player2, player1).

% symbol(+Piece,-Symbol)
% Translates the piece to a visible symbol on the board
symbol(empty,'-|-') :- !.
symbol(notused,'  ') :- !.
symbol(player1,'1') :- !.
symbol(player2,'2') :- !.     
symbol(W,'W') :- !.
symbol(B,'B') :- !.
symbol(n,'X') :- !.

% initial_state(+Size, -GameState)
% Initializes the initial state of the game based on the given board size.
initial_state(Size,[Board,_,_]) :-
    board(Size,Board).

% display_column_numbering(+ColumnNumber, +TotalNumberOfColumns)
% Displays column numbering from 1 to Max on the game board.
display_column_numbering(Max, Max):-
    format('| ~d |', [Max]), !.
display_column_numbering(1, Max):-
    write('   | 1 '),
    display_column_numbering(2, Max), !.
display_column_numbering(N, Max):-
    N > 9,
    format('| ~d ', [N]),
    Next is N + 1,
    display_column_numbering(Next, Max).
display_column_numbering(N, Max):-
    format('| ~d ', [N]),
    Next is N + 1,
    display_column_numbering(Next, Max).

% display_line(+TotalNumberOfColumns)
% Generates and prints a horizontal line separator for the game board.
display_line(0):-
    write('---|\n'), !.
display_line(N):-
    write('---|'),
    N1 is N - 1,
    display_line(N1).

% display_rows(+Board, +Row, +TotalRows, +Columns)
% Displays the rows of the game board with their respective elements.
display_rows(_, LineNumber, Rows, _):- 
    LineNumber > Rows, nl, !.
display_rows([Line|Rest], LineNumber, Rows, Columns):-
    LineNumber > 9,
    format(' ~d |', [LineNumber]),
    display_elements(Line),nl,
    display_line(Columns),
    NextLineNumber is LineNumber + 1,
    display_rows(Rest, NextLineNumber, Rows, Columns), !.
display_rows([Line|Rest], LineNumber, Rows, Columns):-
    format(' ~d |', [LineNumber]),
    display_elements(Line),nl,
    display_line(Columns),
    NextLineNumber is LineNumber + 1,
    display_rows(Rest, NextLineNumber, Rows, Columns).

% display_elements(+Line)
% Displays the elements of a particular row in the game board.
display_elements([]).
display_elements([CurrentElement|Rest]) :-
    symbol(CurrentElement, Symbol),
    format('~w|', [Symbol]),
    display_elements(Rest).

% display_player_turn(+Player)
% Displays a message indicating the player who will make the next move
display_player_turn(Player) :-
    player(Player, PlayerType),
    player_checker(Player, Checker),
    symbol(Checker, Symbol),
    format(' > ~w turn to play! Your checker is:~w', [PlayerType, Symbol]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Lista de moves
% direction_from_checker(+BoardSize, +neutral_pawn_coordinates, +PathCellList)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%