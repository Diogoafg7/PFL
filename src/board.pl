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

% board(+pieces,+Matrix)
% Board structure
% Board Small
board(6, [
    [ empty,   notused,  empty],
    [notused,  empty,  notused],
    [notused, notused, notused]
]).

% Board Medium
board(15, [
    [empty, empty, empty, notused, empty],
    [empty, empty, notused, notused, empty],
    [empty, notused, notused, notused, empty],
    [ notused, notused, notused, notused, empty],
    [notused, notused, notused, notused, notused]
]).


% Board Large
board(28, [
    [empty, empty, empty, notused, empty, empty, empty],
    [empty, empty, empty, notused, notused, empty, empty],
    [empty, empty, notused, notused, notused, empty, empty],
    [empty, empty, notused, notused, notused, notused, empty],
    [empty,notused, notused, notused, notused, notused, empty],
    [empty, notused, notused, notused, notused, notused, notused],
    [notused, notused, notused, notused, notused, notused, notused]
]).

% board(+Cells, +Columns, +Rows)
% Board structure
board(6,7,3).
board(15,7,5).
board(28,7,7).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Lista de moves
% moves_from_neutral(+BoardSize, +neutral_pawn_coordinates, +PathCellList)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate that presents a list with coordinates of cells that can be reached
% in a given direction from a predefined cell, on a fixed size board

% Board Size 3
moves_from_neutral(3, 1-2, [2-1, 2-3, 3-1, 3-3]).
moves_from_neutral(3, 2-1, [1-2, 2-3, 3-1, 3-2]).
moves_from_neutral(3, 2-3, [1-2, 2-1, 3-2, 3-3]).
moves_from_neutral(3, 3-1, [1-2, 2-1, 3-2, 3-3]).
moves_from_neutral(3, 3-2, [2-1, 2-3, 3-1, 3-3]).
moves_from_neutral(3, 3-3, [1-2, 2-3, 3-1, 3-2]).

% Board Size 5
moves_from_neutral(5, 1-4, [2-3, 2-4, 3-2, 3-4, 4-1, 4-4, 5-1, 5-5]).
moves_from_neutral(5, 2-3, [1-4, 2-4, 3-2, 3-3, 4-1, 4-3, 5-1, 5-4]).
moves_from_neutral(5, 2-4, [1-4, 2-3, 3-3, 3-4, 4-2, 4-4, 5-2, 5-5]).
moves_from_neutral(5, 3-2, [1-4, 2-3, 3-3, 3-4, 4-1, 4-2, 5-1, 5-3]).
moves_from_neutral(5, 3-3, [2-3, 2-4, 3-2, 3-4, 4-2, 5-2, 4-3, 5-4]).
moves_from_neutral(5, 3-4, [1-4, 2-4, 3-2, 3-3, 4-3, 5-3, 4-4, 5-5]).
moves_from_neutral(5, 4-1, [1-4, 2-3, 3-2, 5-1, 5-2, 4-2, 4-3, 4-4]).
moves_from_neutral(5, 4-2, [4-1, 4-3, 4-4, 5-2, 5-3, 3-2, 3-3, 2-4]).
moves_from_neutral(5, 4-3, [4-1, 4-2, 4-4, 5-3, 5-4, 3-3, 3-4, 2-3]).
moves_from_neutral(5, 4-4, [4-1, 4-2, 4-3, 5-4, 5-5, 1-4, 2-4, 3-4]).
moves_from_neutral(5, 5-1, [5-2, 5-3, 5-4, 5-5, 1-4, 2-3, 3-2, 4-1]).
moves_from_neutral(5, 5-2, [5-1, 5-3, 5-4, 5-5, 4-1, 4-2, 2-4, 3-3]).
moves_from_neutral(5, 5-3, [5-1, 5-2, 5-4, 5-5, 4-2, 4-3, 3-2, 3-4]).
moves_from_neutral(5, 5-4, [5-1, 5-2, 5-3, 5-5, 4-3, 4-4, 3-3, 2-3]).
moves_from_neutral(5, 5-5, [5-1, 5-2, 5-3, 5-4, 4-4, 3-4, 2-4, 1-4]).

% Board Size 7
moves_from_neutral(7, 1-4, [2-4, 2-5, 3-3, 3-5, 4-3, 4-6, 5-2, 5-6, 6-2, 6-7, 7-1, 7-7]).
moves_from_neutral(7, 2-4, [1-4, 2-5, 3-3, 3-4, 4-3, 5-2, 4-5, 5-5, 6-2, 6-6, 7-1, 7-6]).
moves_from_neutral(7, 2-5, [1-4, 2-4, 3-4, 3-5, 4-6, 5-6, 4-4, 5-3, 6-3, 6-7, 7-2, 7-7]).
moves_from_neutral(7, 3-3, [1-4, 2-4, 3-4, 3-5, 4-3, 4-6, 4-4, 5-4, 6-2, 6-5, 7-1, 7-5]).
moves_from_neutral(7, 3-4, [2-4, 2-5, 3-3, 3-5, 4-5, 5-5, 4-4, 5-3, 6-3, 6-6, 7-2, 7-6]).
moves_from_neutral(7, 3-5, [1-4, 2-5, 3-3, 3-4, 4-6, 5-6, 4-5, 5-4, 6-4, 6-7, 7-3, 7-7]).
moves_from_neutral(7, 4-3, [1-4, 2-4, 3-3, 5-2, 5-3, 4-4, 4-5, 4-6, 6-2, 6-4, 7-1, 7-4]).
moves_from_neutral(7, 4-4, [4-3, 4-5, 4-6, 5-3, 5-4, 3-3, 3-4, 2-5, 6-3, 6-5, 7-2, 7-5]).
moves_from_neutral(7, 4-5, [4-3, 4-4, 4-6, 5-4, 5-5, 3-4, 3-5, 2-4, 6-4, 6-6, 7-3, 7-6]).
moves_from_neutral(7, 4-6, [4-3, 4-4, 4-5, 5-5, 5-6, 1-4, 2-5, 3-5, 6-5, 6-7, 7-4, 7-7]).
moves_from_neutral(7, 5-2, [5-3, 5-4, 5-5, 5-6, 1-4, 2-4, 3-3, 4-3, 6-2, 6-3, 7-1, 7-3]).
moves_from_neutral(7, 5-3, [5-2, 5-4, 5-5, 5-6, 4-3, 4-4, 2-5, 3-4, 6-3, 6-4, 7-2, 7-4]).
moves_from_neutral(7, 5-4, [5-2, 5-3, 5-5, 5-6, 4-4, 4-5, 3-3, 3-5, 6-4, 6-5, 7-3, 7-5]).
moves_from_neutral(7, 5-5, [5-2, 5-3, 5-4, 5-6, 4-5, 4-6, 3-4, 2-4, 6-5, 6-6, 7-4, 7-6]).
moves_from_neutral(7, 5-6, [5-2, 5-3, 5-4, 5-5, 4-6, 3-5, 2-5, 1-4, 6-6, 6-7, 7-5, 7-7]).
moves_from_neutral(7, 6-2, [6-3, 6-4, 6-5, 6-6, 6-7, 2-4, 3-3, 4-3, 5-2, 1-4, 7-1, 7-2]).
moves_from_neutral(7, 6-3, [6-2, 6-4, 6-5, 6-6, 6-7, 7-2, 7-3, 5-2, 5-3, 4-4, 2-5, 3-4]).
moves_from_neutral(7, 6-4, [6-2, 6-3, 6-5, 6-6, 6-7, 7-3, 7-4, 5-3, 5-4, 4-3, 4-5, 3-5]).
moves_from_neutral(7, 6-5, [6-2, 6-3, 6-4, 6-6, 6-7, 7-4, 7-5, 5-4, 5-5, 4-4, 4-6, 3-3]).
moves_from_neutral(7, 6-6, [6-2, 6-3, 6-4, 6-5, 6-7, 7-5, 7-6, 5-5, 5-6, 4-5, 3-4, 2-4]).
moves_from_neutral(7, 6-7, [6-2, 6-3, 6-4, 6-5, 6-6, 7-6, 7-7, 5-6, 1-4, 4-6, 3-5, 2-5]).
moves_from_neutral(7, 7-1, [7-2, 7-3, 7-4, 7-5, 7-6, 7-7, 1-4, 2-4, 3-3, 4-3, 5-2, 6-2]).
moves_from_neutral(7, 7-2, [7-1, 7-3, 7-4, 7-5, 7-6, 7-7, 6-2, 6-3, 5-3, 3-4, 4-4, 2-5]).
moves_from_neutral(7, 7-3, [7-1, 7-2, 7-4, 7-5, 7-6, 7-7, 6-3, 6-4, 5-4, 3-5, 4-5, 5-2]).
moves_from_neutral(7, 7-4, [7-1, 7-2, 7-3, 7-5, 7-6, 7-7, 6-4, 6-5, 5-5, 4-3, 4-6, 5-3]).
moves_from_neutral(7, 7-5, [7-1, 7-2, 7-3, 7-4, 7-6, 7-7, 6-5, 6-6, 5-6, 4-4, 3-3, 5-4]).
moves_from_neutral(7, 7-6, [7-1, 7-2, 7-3, 7-4, 7-5, 7-7, 6-6, 6-7, 5-5, 4-5, 3-4, 2-4]).
moves_from_neutral(7, 7-7, [7-1, 7-2, 7-3, 7-4, 7-5, 7-6, 6-7, 5-6, 4-6, 3-5, 2-5, 1-4]).


% other_player(+CurrentPlayer,-NextPlayer)
% Change player turn
other_player(player1, player2).
other_player(player2, player1).

% symbol(+Piece,-Symbol)
% Translates the piece to a visible symbol on the board
symbol(empty,'-|-') :- !.
symbol(notused,' O ') :- !.
symbol(player1,'1') :- !.
symbol(player2,'2') :- !.     
symbol(W,'W') :- !.  % white player
symbol(B,'B') :- !.  % black player
symbol(n,'X') :- !.  % neutral pawn

% initial_state(+Size, -GameState)
% Initializes the initial state of the game based on the given board size.
% GameState has the format "[Board, Player, MoveNumber]".

initial_state(Size, [Board, _, MoveNumber]) :-
    % Generate the game board based on the size
    board(Size, Board),


    % Set the initial move number
    MoveNumber = 1. % You can start with 1 or any other value as per your games rules


% display_column_numbering(+ColumnNumber, +TotalNumberOfColumns)
% Displays column numbering from 1 to Max on the game board.
display_column_numbering(Max, Max):-
    format('~d\n ', [Max]), !.
display_column_numbering(1, Max):-
    write('\n    1   '),
    display_column_numbering(2, Max), !.
display_column_numbering(N, Max):-
    N > 9,
    format('~d  ', [N]),
    Next is N + 1,
    display_column_numbering(Next, Max).
display_column_numbering(N, Max):-
    format('~d  ', [N]),
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

