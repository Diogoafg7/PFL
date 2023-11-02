% name_of(+Player, -Name)
% Find the Players name
:- dynamic name_of/2.

% difficulty(+Bot,-Difficulty)
% Find the Bot difficulty
:- dynamic difficulty/2.

% black(+Coordinate)
% Find black piece coordinate
:- dynamic black/1.

% white(+Coordinate)
% Find white piece coordinate
:- dynamic white/1.

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

% piece_info(?Type,?Player,+Piece)
% It allows to generalize the type of piece and to know the player that uses it
piece_info(white, player1, white).
piece_info(black, player2, black).
piece_info(selected, player1, selected1).
piece_info(selected, player2, selected2).
piece_info(notused, neutral).
piece_info(empty, neutral).

% other_player(+CurrentPlayer,-NextPlayer)
% Change player turn
other_player(player1, player2).
other_player(player2, player1).

% symbol(+Piece,-Symbol)
% Translates the piece to a visible symbol on the board
symbol(empty,' ') :- !.
symbol(notused,'-') :- !.
symbol(selected1,'x1') :- !.
symbol(selected2,'x2') :- !.
symbol(white,'W') :- !.
symbol(black,'B') :- !.
