:- use_module(library(lists)).
:- use_module(library(random)).
:- consult(configurations).
:- consult(board).

% validate_move(+Board,+CoordsOrigin,+CoordsDestination)
% Checks if the move is valid or not
validate_move(GameState, ColI-RowI,ColF-RowF) :-
    [Board,Player,_] = GameState,
    in_bounds(Board,ColI-RowI), in_bounds(Board,ColF-RowF), 
    position(Board, ColI-RowI,PieceI), position(Board, ColF-RowF, PieceF),
    \+(piece_info(PieceI, neutral)), piece_info(PieceF, neutral),
    piece_info(PieceType,Player,PieceI),              
    valid_direction(PieceType,ColI-RowI,ColF-RowF),  
    \+path_obstructed(Board,ColI-RowI,ColF-RowF).

% horizontal_move(+PosOrigin, +PosDestination)
% Checks if the move is purely horizontal
horizontal_move(Col-_, Col-_) :- !, fail.
horizontal_move(_-Row, _-Row).

% diagonal_move(+PosOrigin,+PosDestination)
% Checks if the move is diagonal
diagonal_move(ColI-RowI,ColF-RowF) :-
    ColDif is ColF-ColI, RowDif is RowF-RowI,  
    abs(ColDif,AbsDif), abs(RowDif,AbsDif). % if both differences are equal we can say the move is diagonal


% valid_direction(+PieceType,+PosOrigin,+PosDestination)
% Checks if the direction of the move is valid
valid_direction(block,ColI-RowI,ColF-RowF) :-
    horizontal_move(ColI-RowI,ColF-RowF); diagonal_move(ColI-RowI,ColF-RowF).


% path_obstructed(+Board,+PosOrigin,+PosDestination)
% Checks if there is a piece between the two positions of the move
path_obstructed(Board, ColI-RowI,ColF-RowF) :-
    DeltaCol is ColF-ColI, DeltaRow is RowF-RowI,
    move_direction(DeltaCol-DeltaRow,HorDir,VerDir),
    \+path_obstructedAux(Board, ColI-RowI,ColF-RowF,HorDir-VerDir).

% path_obstructedAux(+Board,+PosOrigin,+PosDestination,+Direction)
% Auxiliary function of path_obstructed.
path_obstructedAux(_,Col-Row,Col-Row,_) :- !.
path_obstructedAux(Board,ColI-RowI,ColF-RowF,HorDir-VerDir) :-
    NewCol is ColI + HorDir, NewRow is RowI + VerDir,
    position(Board,NewCol-NewRow,Piece),
    piece_info(Piece, neutral), !,
    path_obstructedAux(Board,NewCol-NewRow,ColF-RowF,HorDir-VerDir).

% move_direction(+MoveVector,-HorDir,-VerDir) :-
% Given a move, gives the horizontal and vertical direction with both forming a unit vector
move_direction(DeltaCol-0,-1, 0) :-         %Left Move
    DeltaCol < 0, !.
move_direction(DeltaCol-0,1, 0) :-          %Right Move
    DeltaCol > 0, !.
move_direction(DeltaCol-DeltaRow,-1,-1) :-  %Top-Left move
    (DeltaCol < 0, DeltaRow < 0), !.
move_direction(DeltaCol-DeltaRow,1,-1) :-   %Top-Right move
    (DeltaCol > 0, DeltaRow < 0), !.
move_direction(DeltaCol-DeltaRow,-1,1) :-   %Bottom-Left move
    (DeltaCol < 0, DeltaRow > 0), !.
move_direction(DeltaCol-DeltaRow,1,1) :-    %Bottom-Right move
    (DeltaCol > 0, DeltaRow > 0), !.


% move(+GameState, +Move, -NewGameState)
% Moves a piece
move(GameState, ColI-RowI-ColF-RowF, NewGameState):-                       
    [Board,Player,_,TotalMoves] = GameState,
    position(Board,ColI-RowI,Piece),
    put_piece(Board, ColI-RowI, empty, NewBoard1),
    put_piece(NewBoard1, ColF-RowF, Piece, NewBoard),
    other_player(Player, NewPlayer),
    forced_moves(NewBoard, NewPlayer, NewForcedMoves),
    NewTotalMoves is TotalMoves + 1,
    NewGameState = [NewBoard,NewPlayer,NewForcedMoves,NewTotalMoves].

% valid_moves(+GameState, +Player, -ListOfMoves)
% Gets all the valid moves of the given player
valid_moves(GameState, _, ListOfMoves):-
    findall(ColI-RowI-ColF-RowF, validate_move(GameState, ColI-RowI, ColF-RowF), ListOfMoves),
    \+length(ListOfMoves, 0), !.
valid_moves(GameState, Player, ListOfMoves):-
    [Board,Player,_] = GameState,
    findall(ColI-RowI-ColF-RowF, validate_move([Board,Player,TotalMoves],ColI-RowI,ColF-RowF),ListOfMoves).



