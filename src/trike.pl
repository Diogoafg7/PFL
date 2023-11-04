:- use_module(library(lists)).
:- use_module(library(random)).
:- consult(menu).
:- consult(board).
:- consult(utils).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% New updates %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% play/0
% Starts the game and clears data when it ends 
play :-
    game_setup(GameState), !,
    game_cycle(GameState),
    clear_data.

% game_cycle(+GameState)
% Loop that keeps the game running
game_cycle(GameState):-
    game_over(GameState, Winner), !,
    display_game(GameState),
    show_winner(GameState, Winner).
game_cycle(GameState):-
    display_game(GameState),
    print_turn(GameState),
    choose_move(GameState, Move),
    move(GameState, Move, NewGameState), !,
    game_cycle(NewGameState).

% display_game(+GameState)
% Prints the board
display_game([Board,_,_,_]) :-
    clear_console,
    length(Board, Size),
    display_header(1, Size),
    display_bar(Size),
    display_rows(Board, 1, Size).

% print_turn(+GameState)
% Prints a message declaring whose turn it is
print_turn([_, Player, _, _]):-
    name_of(Player, Name),
    format('Player ~a, is your turn!\n', [Name]), !.

% game_over(+GameState)
% Checks if the game has reached a ending state
game_over([Board,_,_]) :-
    length(Board, Rows),
    board(Size, _, Rows),
    neutral_pawn_coordinates(NeutralRow-NeutralCol),!,
    \+ at_least_one_cell_empty(Board, Size, NeutralRow-NeutralCol).

% find_out_winner(+GameState, -Winner)
% Finds the winner given the game state
find_out_winner([Board, Player,_], Winner) :-
    neutral_pawn_coordinates(NeutralRow-NeutralCol),
    length(Board, Rows),
    board(Size, _, Rows),
    other_player(Player, OtherPlayer),
    asserta(player_score(Player, 1)),
    asserta(player_score(OtherPlayer, 0)),
    calculate_total_score(Board, Size, NeutralRow-NeutralCol, Player, OtherPlayer),
    decide_the_winner(Player, OtherPlayer, Winner).

% game_over(+GameState)
% Checks if the game has reached a ending state
game_over([Board,_,_]) :-
    length(Board, Rows),
    board(Size, _, Rows),
    neutral_pawn_coordinates(NeutralRow-NeutralCol),!,
    \+ at_least_one_cell_empty(Board, Size, NeutralRow-NeutralCol).

% find_out_winner(+GameState, -Winner)
% Finds the winner given the game state
find_out_winner([Board, Player,_], Winner) :-
    neutral_pawn_coordinates(NeutralRow-NeutralCol),
    length(Board, Rows),
    board(Size, _, Rows),
    other_player(Player, OtherPlayer),
    asserta(player_score(Player, 1)),
    asserta(player_score(OtherPlayer, 0)),
    calculate_total_score(Board, Size, NeutralRow-NeutralCol, Player, OtherPlayer),
    decide_the_winner(Player, OtherPlayer, Winner).

% choose_move(+GameState,-Move)
% A human player chooses a move
choose_move([Board,Player,MoveNumber], Row-Col) :-
    \+difficulty_level(Player, _),
    repeat,
    get_move([Board,Player,MoveNumber],Row-Col),
    validate_move([Board,Player,MoveNumber], Row-Col), !.
choose_move([Board,Player,MoveNumber], Move):-
    difficulty(Player, Level),                  
    choose_move([Board,Player,MoveNumber], Player, Level, Move), !. 

% get_option(+Min,+Max,+Context,-Value)
% Unifies Value with the value given by user input between Min and Max when asked about Context
get_option(Min,Max,Context,Value):-
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    repeat,
    read_number(Value),
    between(Min, Max, Value), !.

% get_move(+Board,-Coordinate)
% Unifies Coordinate with a valid coordinate given by input within the Board
get_move(Board, Col1-Row1-Col2-Row2):-
    length(Board, Size),
    get_option(1, Size, 'Origin column', Col1),
    get_option(1, Size, 'Origin row', Row1),
    get_option(1, Size, 'Destination column', Col2),
    get_option(1, Size, 'Destination row', Row2).

% validate_move(+GameState,-Coordinate)
% Validates that the entered coordinates correspond to a valid position for inserting a checker
validate_move([Board, _, 1], Row-Col) :-
    is_cell_empty(Board, Row-Col).
validate_move([_,_,2],_-_) :-true.
validate_move([Board, _, 1], Row-Col) :-
    \+ is_cell_empty(Board, Row-Col),nl,
    write('Invalid cell chosen. The cell has to be empty, in a valid direction and the path from the neutral pawn'),nl,
    write('to the cell choosen cannot be obstructed. Please choose a valid cell!'),nl,nl,
    fail.
validate_move([Board,_,_], Row-Col) :- 
    is_cell_empty(Board, Row-Col),
    is_valid_direction_not_obstructed(Board, Row, Col).   

% move(+GameState, +Move, -NewGameState)
% Moves a piece
move([Board,Player,1], Row-Col, NewGameState) :-
    put_neutral_pawn(Board, Row-Col, NewBoard),
    other_player(Player, NewPlayer),
    NewMoveNumber is 2,
    NewGameState = [NewBoard,NewPlayer,NewMoveNumber].
move([Board,Player,2],_-_,NewGameState) :-
    swap_sides_decision(Choice),
    Choice = 'y' ->
        write('You chose to swap sides!'), nl,
        swap_sides,
        other_player(Player, NextPlayer),
        NewGameState = [Board,NextPlayer,3]
    ;
        write('You chose not to swap sides. Proceed with the next move.'), nl,
        NewGameState = [Board,Player,3].   
move([Board,Player,MoveNumber], Row-Col, NewGameState):-                       
    other_player(Player, NewPlayer),
    player_checker(NewPlayer, Checker),
    neutral_pawn_coordinates(NeutralRow-NeutralCol),
    replace_neutral_pawn(Board, NeutralRow-NeutralCol, Checker, NewBoard1),
    put_neutral_pawn(NewBoard1, Row-Col, NewBoard),
    NewMoveNumber is MoveNumber + 1,
    NewGameState = [NewBoard,NewPlayer,NewMoveNumber].

% swap_side(+CurrentPlayer)
% Swap the symbols that represent each player
swap_sides :-
    retractall(player_checker(_,_)),
    asserta(player_checker(player1, W)),
    asserta(player_checker(player2, B)).

% is_cell(+Board, -Row, -Col)
% Checks whether the position with the respective coordinates is a cell
is_cell_empty(Board, Row-Col) :-
    nth1(Row, Board, RowList), 
    nth1(Col, RowList, cell).

% is_valid_direction_not_obstructed(+Board, +Row, +Col)
% Validates if the direction is valid and not obstructed.
is_valid_direction_not_obstructed(Board, Row, Col) :-
    length(Board, Rows),
    board(Size,_,Rows),
    neutral_pawn_coordinates(NeutralRow-NeutralCol),
    direction_from_checker(Size, NeutralRow-NeutralCol, PathList),
    memberchk(Row-Col, PathList), !,
    is_not_obstructed(Board, Row-Col, PathList).

% is_not_obstructed(+Board, +Row-Col, +PathList)
% Checks whether the path is not obstructed
is_not_obstructed(_, Row-Col, [Row-Col|_]).
is_not_obstructed(Board, Row-Col, [H|T]) :-
    is_cell_empty(Board, H),
    is_not_obstructed(Board, Row-Col, T).

% put_neutral_pawn(+Board, +Row-Col, -NewBoard)
% Places the neutral pawn at the given Row-Col position
put_neutral_pawn(Board, Row-Col, NewBoard) :-
    RowIndex is Row - 1, ColIndex is Col - 1,
    nth0(RowIndex,Board,Line),
    replace(ColIndex, n, Line, NewLine),
    replace(RowIndex, NewLine, Board, NewBoard),
    set_neutral_pawn_coordinate(Row-Col).

% replace_neutral_pawn(+Board, +NeutralRow-NeutralCol, +Checker, -NewBoard)
% Replaces the neutral pawn at the position NeutralRow-NeutralCol with the provided Checker
replace_neutral_pawn(Board, NeutralRow-NeutralCol, Checker, NewBoard):-
    RowIndex is NeutralRow - 1, ColIndex is NeutralCol - 1,
    nth0(RowIndex,Board,Line),
    replace(ColIndex, Checker, Line, NewLine),
    replace(RowIndex, NewLine, Board, NewBoard). 

% replace(+Index,+Element,+List,-Result)
% Unifies Result with the list resulting from replace the element at Index of List by Element
replace(Index, Element, List, Result) :-
  nth0(Index, List, _, R),
  nth0(Index, Result, Element, R).

% at_least_one_cell_empty(+Board, +Size, +NeutralRow-NeutralCol)
% Checks for the presence of at least one empty cell around the neutral pawn
at_least_one_cell_empty(Board, Size, NeutralRow-NeutralCol) :-
    direction_from_checker(Size, NeutralRow-NeutralCol, List),
    nth1(1, List, FirstElement),
    is_cell_empty(Board, FirstElement), !.

% calculate_total_score(+Board, +Size, +NeutralRow-NeutralCol, +Player, +OtherPlayer)
% Determines the total score
calculate_total_score(Board, Size, NeutralRow-NeutralCol, Player, OtherPlayer) :-
    direction_from_checker(Size, NeutralRow-NeutralCol, List),
    add_1_point(Board, List, Player, OtherPlayer).

% add_1_point(+Board, +List, +Player, +OtherPlayer)
% Adds one point based on the checker type
add_1_point(Board, List, Player, OtherPlayer) :-
    nth1(1, List, FirstElementCoordinate),
    check_checker_type(Board, FirstElementCoordinate, Type),
    add_score_due_to_type(Type, Player, OtherPlayer).

% check_checker_type(+Board, +Row-Col, -Type)
% Retrieves the type of the checker at the specified Row and Column
check_checker_type(Board, Row-Col,Type) :-
    nth1(Row, Board, RowList), 
    nth1(Col, RowList, Type).

% add_score_due_to_type(+Type, +Player, +OtherPlayer)
% Adjusts the scores of a player based on the type of the checker
add_score_due_to_type(Type, Player, OtherPlayer) :-
    player_checker(Player, Checker),
    player_score(Player, Score),
    player_score(OtherPlayer, OtherScore),
    (
        Type == Checker ->
            NewScore is Score + 1,
            retract(player_score(Player,_)),
            asserta(player_score(Player, NewScore))
        ;
            NewOtherScore is OtherScore + 1,
            retract(player_score(OtherPlayer,_)),
            asserta(player_score(OtherPlayer,NewOtherScore))       
    ).

% decide_the_winner(+Player, +OtherPlayer, -Winner)
% Determines the winner based on the players scores.
decide_the_winner(Player, OtherPlayer, Winner) :-
    player_score(Player, Score),
    player_score(OtherPlayer, OtherScore),
    (
        Score > OtherScore ->
            Winner = Player
        ;
            Winner = OtherPlayer
    ).

% congratulate(+Winner)
% Displays a congratulatory message to the Winner
congratulate(Winner) :-
    player(Winner, WinnerPlayer),
    player_score(Winner, Score),
    format(' > The ~w won with a score of ~d. Congratulations!', [WinnerPlayer,Score]), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Movimento Bot
% choose_move(+GameState,+Player,+Level,-Move)
% Bot random player. Makes a list of possible moves and select a random one
choose_move(GameState, Player, 1, Row-Col):- 
    valid_moves(GameState, Player, ListOfMoves), % direction_from_checker
    random_member(Row-Col, ListOfMoves).
