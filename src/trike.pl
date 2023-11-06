:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system)).
:- consult(menu).
:- consult(board).
:- consult(utils).

% Starts the game and clears data when it ends 
play :-
    write('\e[H\e[2J'),
    game_setup(GameState),!,
    game_cycle(GameState),
    clear_data.
    
% game_cycle(+GameState)
% Loop that keeps the game running
game_cycle(GameState):-
    game_over(GameState), !,
    display_game(GameState),
    show_winner(player1, player2).
game_cycle(GameState):-
    display_game(GameState),
    print_turn(GameState),
    choose_move(GameState, Move),
    move(GameState, Move, NewGameState), !,
    game_cycle(NewGameState).

% display_game(+GameState)
% Prints the board
display_game([Board,_,_]) :-
    clear_console,
    length(Board, Size),
    display_column_numbering(1, Size),
    display_line(Size),
        display_rows(Board, 1, Size, Size).

% print_turn(+GameState)
% Prints a message declaring whose turn it is
print_turn([_, Player, _]):-
    name_of(Player, Name),
    format('Player ~a, is your turn!\n', [Name]), !.


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

% choose_move(+GameState, +Player, +Level, -Move)
% Selects a random move for the computer
choose_move(GameState, Player, 1,  Row-Col):-
    valid_moves(GameState, Player, ListOfMoves),
    random_member(Row-Col, ListOfMoves).

% choose_move(+GameState, +Player, +Level, -Move)
% Selects a greedy move for the computer
choose_move([Board,_,1], Player, 2, Row-Col) :-
    length(Board, Rows),
    board(_, Columns, Rows),
    Row is 1,
    Col is Columns // 2 + 1.
choose_move(GameState, Player, 2, Row-Col):-
	valid_moves(GameState, Player, ListOfMoves), 
    other_player(Player, OtherPlayer),
    check_ListOfMoves_Size(ListOfMoves, Size),
    Size == 1 ->
        ListOfMoves = [Row-Col|_]
    ;
    check_possibility_of_winning(GameState, Player, OtherPlayer, ListOfMoves, Row-Col, Return),
    Return == break ->
        true
    ;
	findall(Value-Coordinate, ( member(Coordinate, ListOfMoves), 
                                move(GameState, Coordinate, NewGameState),
                                retract(player_score(Player,_)),
                                asserta(player_score(Player, 1)), 
                                value(NewGameState,Player, Value)
                                ), Pairs),
    sort(Pairs, SortedPairs),
    last(SortedPairs, Max-_),
    findall(Coordinates, member(Max-Coordinates, SortedPairs), MaxCoordinates),
    random_member(Row-Col, MaxCoordinates).

% valid_moves(+GameState, +Player, -ListOfMoves)
% Calculates a list of available moves for the current game state.
valid_moves(GameState, _, ListOfMoves):-
    findall(Row-Col, validate_move(GameState,Row-Col),ListOfMoves),
    \+length(ListOfMoves, 0), !.
valid_moves(GameState, Player, ListOfMoves):-
    [Board,Player,MoveNumber] = GameState,
    findall(Row-Col, validate_move([Board,Player,MoveNumber],Row-Col),ListOfMoves).

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
    get_option(1, Size, 'Destination row', Row2),
    validate_move([Board,Player,MoveNumber], Row-Col), !.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary Rules and Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% valid_get_move_coordinate(+Value,-Max)
% Checks if the entered value is between 1 and the maximum
valid_get_move_coordinate(Value, Max) :-
    integer(Value),
    Value >= 1,
    Value =< Max.

% set_neutral_pawn_coordinate(-Coordinate)
set_neutral_pawn_coordinate(Row-Col) :-
    retractall(neutral_pawn_coordinates(_)),
    asserta(neutral_pawn_coordinates(Row-Col)).

% swap_sides_decision(+Choice)
% Ask the player if he wants to switch sides and save the answer
swap_sides_decision(Choice) :-
    write('Do you want to swap sides? (y/n): '),
    read(Input),
    (
        Input == 'y' ->
            Choice = 'y'
        ;
        Input == 'n' ->
            Choice = 'n'
        ;
        write('Invalid choice. Please enter either "y." or "n."'), nl,
        swap_sides_decision(Choice)
    ).

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
    moves_from_neutral(Size, NeutralRow-NeutralCol, PathList),
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
    moves_from_neutral(Size, NeutralRow-NeutralCol, List),
    nth1(1, List, FirstElement),
    is_cell_empty(Board, FirstElement), !.

% calculate_total_score(+Board, +Size, +NeutralRow-NeutralCol, +Player, +OtherPlayer)
% Determines the total score
calculate_total_score(Board, Size, NeutralRow-NeutralCol, Player, OtherPlayer) :-
    first_elements_list(Size, NeutralRow-NeutralCol, FirstElementsList),
    add_points_due_to_checker_type(Board, Player, OtherPlayer, FirstElementsList).

% first_elements_list(+Size, +NeutralRow-NeutralCol, -FirstElementsList)
% Retrieves the list of first elements from the 'moves_from_neutral' predicate for a given neutral position.
first_elements_list(Size, NeutralRow-NeutralCol, FirstElementsList) :-
    bagof(FirstElement, Args^(moves_from_neutral(Size, NeutralRow-NeutralCol, Args), nth1(1, Args, FirstElement)), FirstElementsList).

% add_points_due_to_checker_type(+Board, +Player, +OtherPlayer, +List)
% Adds points based on the checker type for a list of cell positions.
add_points_due_to_checker_type(_,_,_,[]).
add_points_due_to_checker_type(Board, Player, OtherPlayer, [H|T]) :-
    check_checker_type(Board, H, Type),
    add_score_due_to_type(Type, Player, OtherPlayer),
    add_points_due_to_checker_type(Board, Player, OtherPlayer, T).

% check_checker_type(+Board, +Row-Col, -Type)
% Retrieves the type of the checker at the specified Row and Column
check_checker_type(Board, Row-Col, Type) :-
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

% show_winner(+Player, +OtherPlayer)
% Determines the winner and displays a congratulatory message.
show_winner(Player, OtherPlayer) :-
    player_score(Player, Score),
    player_score(OtherPlayer, OtherScore),
    (
        Score > OtherScore ->
            Winner = Player
        ;
            Winner = OtherPlayer
    ),
    player(Winner, WinnerPlayer),
    format(' > The ~w won with a score of ~d. Congratulations!', [WinnerPlayer, Score]), nl.

% check_possibility_of_winning(+GameState, +Player, +OtherPlayer, +List, -Row-Col, -Return)
% Checks the possibility of winning based on a list of coordinates of valid moves                                                                          
check_possibility_of_winning(_,_,_,[],_,_).
check_possibility_of_winning([Board, Player, _], Player, OtherPlayer, [H|T], Row-Col, Return) :-
    length(Board, Rows),
    board(Size, _, Rows),
    at_least_one_cell_empty(Board, Size, H),
    Return = continue.
check_possibility_of_winning([Board, Player, _], Player, OtherPlayer, [H|T], Row-Col, Return) :-
    length(Board, Rows),
    board(Size, _, Rows),
    \+ at_least_one_cell_empty(Board, Size, H),
    asserta(player_score(Player,1)),
    calculate_preview_score(Board, Size, H, Player, OtherPlayer),
    player_score(Player, Score),
    Score > 0 ->
        Row-Col = H,
        Return = break
    ;
    check_possibility_of_winning([Board, Player, _], Player, OtherPlayer, T, Row-Col, Return).

% calculate_preview_score(+Board, +Size, +NeutralRow-NeutralCol, +Player, +OtherPlayer)
% Calculates the score based on the game board and a specific coordinate
calculate_preview_score(Board, Size, NeutralRow-NeutralCol, Player, OtherPlayer) :-
    first_elements_list(Size, NeutralRow-NeutralCol, FirstElementsList),
    add_subtract_due_to_checker_type(Board, Player, OtherPlayer, FirstElementsList).

% add_subtract_due_to_checker_type(+Board, +Player, +OtherPlayer, +List)
% Adds or subtracts score based on the checker type encountered in the provided list
add_subtract_due_to_checker_type(_,_,_,[]).
add_subtract_due_to_checker_type(Board, Player, OtherPlayer, [H|T]) :-
    check_checker_type(Board, H, Type),
    add_subtract_score_due_to_type(Type, Player),
    add_subtract_due_to_checker_type(Board, Player, OtherPlayer, T).

% add_subtract_score_due_to_type(+Type, +Player)
% Adjusts the players score based on the type of the encountered checker
add_subtract_score_due_to_type(Type, Player) :-
    player_checker(Player, Checker),
    player_score(Player, Score),
    (
        Type == Checker ->
            NewScore is Score + 1,
            retract(player_score(Player,_)),
            asserta(player_score(Player, NewScore))
        ;
            NewScore is Score - 1,
            retract(player_score(OtherPlayer,_)),
            asserta(player_score(OtherPlayer,NewOtherScore))       
    ).

% check_ListOfMoves_Size(+ListOfMoves, +Size)
% Gets a list size
check_ListOfMoves_Size(ListOfMoves, Size):-
    length(LisOfMoves, Size).