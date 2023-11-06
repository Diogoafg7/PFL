:- use_module(library(system)).
:- consult(menu).
:- consult(board).
:- consult(utils).

% Starts the game and clears data when it ends 
play :-
    clear_console,
    game_setup(GameState),!,
    game_cycle(GameState),
    clear_data.
    
% game_cycle(+GameState)
% Loop that keeps the game running
game_cycle(GameState):-
    is_over(GameState, Winner), !,
    display_game(GameState),
    find_who_wins(GameState, Winner),
    show_winner(Winner).
game_cycle(GameState):-
    display_game(GameState),
    make_move(GameState, Move),
    move(GameState, Move, NewGameState), !,
    game_cycle(NewGameState).

% display_game(+GameState)
% Prints the board
display_game([Board,_,_]) :-
    sleep(1),
    clear_console,
    length(Board, Rows),
    board(_,Columns, Rows),
    display_line(Columns),
    display_column_numbering(1, Columns),nl,
    display_line(Columns),
    display_rows(Board, 1, Rows, Columns),
    display_player_turn(Player), nl,nl.

% find_who_wins(+GameState, -Winner)
% Check if there is a winner in the current state of the game
find_who_wins([Board, Player,_], Winner) :-
    neutral_pawn_coordinates(NeutralRow-NeutralCol),
    length(Board, Rows),
    board(Size, _, Rows),
    other_player(Player, OtherPlayer),
    asserta(player_score(Player, 1)),
    asserta(player_score(OtherPlayer, 0)),
    total_score(Board, Size, NeutralRow-NeutralCol, Player, OtherPlayer),
    decide_the_winner(Player, OtherPlayer, Winner).

% make_move(+GameState,-Move)
% A player chooses a move
make_move([Board,Player,NumberMove], Row-Column) :-
    \+difficulty_level(Player, _),
    repeat,
    get_move([Board,Player,NumberMove],Row-Column),
    validate_move([Board,Player,NumberMove], Row-Column), !.
make_move([Board,Player,NumberMove], Move):-
    difficulty(Player, Level),                  
    make_move([Board,Player,NumberMove], Player, Level, Move), !. 

% make_move(+GameState, +Player, +Level, -Move)

% Selects a random move for the computer
make_move(GameState, Player, 1,  Row-Column):-
    valid_moves(GameState, Player, ListOfMoves),
    random_member(Row-Column, ListOfMoves).

% make_move(+GameState, +Player, +Level, -Move)
% Selects a greedy move for the computer
make_move([Board,_,1], Player, 2, Row-Column) :-
    length(Board, Rows),
    board(_, Columns, Rows),
    Row is 1,
    Col is Columns // 2 + 1.
make_move(GameState, Player, 2, Row-Column):-
	valid_moves(GameState, Player, ListOfMoves), 
    other_player(Player, OtherPlayer),
    size_of_list_of_moves(ListOfMoves, Size),
    Size == 1 ->
        ListOfMoves = [Row-Column|_]
    ;
    is_possible_win(GameState, Player, OtherPlayer, ListOfMoves, Row-Column, Return),
    Return == break ->
        true
    ;
	findall(Value-Coordinate, ( member(Coordinate, ListOfMoves), 
                                move(GameState, Coordinate, NewGameState),
                                retract(player_score(Player,_)),
                                asserta(player_score(Player, 1)), 
                                value_of_board(NewGameState,Player, Value)
                                ), Pairs),
    sort(Pairs, SortedPairs),
    last(SortedPairs, Max-_),
    findall(Coordinates, member(Max-Coordinates, SortedPairs), MaxCoordinates),
    random_member(Row-Column, MaxCoordinates).

% valid_moves(+GameState, +Player, -ListOfMoves)
% Calculates a list of available moves for the current game state.
valid_moves(GameState, _, ListOfMoves):-
    findall(Row-Column, validate_move(GameState,Row-Column),ListOfMoves),
    \+length(ListOfMoves, 0), !.
valid_moves(GameState, Player, ListOfMoves):-
    [Board,Player,NumberMove] = GameState,
    findall(Row-Column, validate_move([Board,Player,NumberMove],Row-Column),ListOfMoves).

get_move([_, _, 2], _-_).
get_move([Board, _, _], Row-Column) :-
    length(Board, Rows),
    board(_, Columns, Rows),
    repeat,
    get_valid_coordinate('row', Rows, TempRow, Row),
    get_valid_coordinate('column', Columns, TempCol, Col),
    validate_move([Board,Player,NumberMove], Row-Column), !.

% get_valid_coordinate(+Type, +Max, -TempCoord, -Coord)
% Gets a valid coordinate from the user
get_valid_coordinate(Type, Max, TempCoord, Coord) :-
    format('Select a ~w between 1 and ~d: ', [Type, Max]),
    read(TempCoord),
    (valid_get_move_coordinate(TempCoord, Max) -> Coord = TempCoord
    ; write('Invalid ~w. Please choose a valid ~w.', [Type, Type]), nl, fail).
    

% validate_move(+GameState,-Coordinate)
% Validates that the entered coordinates correspond to a valid position for inserting a checker
validate_move([Board, _, 1], Row-Column) :-
    is_cell_empty(Board, Row-Column).
validate_move([_,_,2],_-_) :-true.
validate_move([Board, _, 1], Row-Column) :-
    \+ is_cell_empty(Board, Row-Column),nl,
    write('Invalid cell chosen. The cell has to be empty, in a valid direction and the path from the neutral pawn'),nl,
    write('to the cell choosen cannot be obstructed. Please choose a valid cell!'),nl,nl,
    fail.
validate_move([Board,_,_], Row-Column) :- 
    is_cell_empty(Board, Row-Column),
    is_not_obstructed_and_valid(Board, Row, Col).   

% move(+GameState, +Move, -NewGameState)
% Moves a piece
move([Board,Player,1], Row-Column, NewGameState) :-
    place_neutral(Board, Row-Column, NewBoard),
    other_player(Player, NewPlayer),
    NewMoveNumber is 2,
    NewGameState = [NewBoard,NewPlayer,NewMoveNumber].
move([Board,Player,2],_-_,NewGameState) :-
    swap_sides_decision(Choice),
    Choice = 'y' ->
        write('You chose to swap pieces!'), nl,
        swap_sides,
        other_player(Player, NextPlayer),
        NewGameState = [Board,NextPlayer,3]
    ;
        write('You chose not to swap pieces. Proceed with the next move.'), nl,
        NewGameState = [Board,Player,3].   
move([Board,Player,NumberMove], Row-Column, NewGameState):-                       
    other_player(Player, NewPlayer),
    player_symbol(NewPlayer, Checker),
    neutral_pawn_coordinates(NeutralRow-NeutralCol),
    replace_neutral_pawn(Board, NeutralRow-NeutralCol, Checker, NewBoard1),
    place_neutral(NewBoard1, Row-Column, NewBoard),
    NewMoveNumber is NumberMove + 1,
    NewGameState = [NewBoard,NewPlayer,NewMoveNumber].


%  is_over(+GameState, -Winner)
% Checks if the game has reached a ending state
is_over([Board,Player,_] , Winner) :-
    length(Board, Rows),
    board(Size, _, Rows),
    neutral_pawn_coordinates(NeutralRow-NeutralCol),!,
    \+ at_least_one_notused(Board, Size, NeutralRow-NeutralCol),
    find_who_wins([Board,Player,_], Winner).

% value_of_board(+GameState, +Player, -Value)
% Calculates the value_of_board of the current board for a specific player based on the position of the neutral pawn
value_of_board([Board,_,_],Player, Value) :-
    neutral_pawn_coordinates(NeutralRow, NeutralCol),
    length(Board, Rows),
    board(Size, _, Rows),
    other_player(Player,OtherPlayer),
    preview_score(Board, Size, NeutralRow-NeutralCol, Player, OtherPlayer),
    player_score(Player, Score),
    Value is Score.  



% show_winner(+Winner)
% Displays the winner of the game
show_winner(Winner) :-
    player(Winner, WinnerPlayer),
    player_score(Winner, Score),
    format("~a wins with ~d points!",[WinnerPlayer,Score]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary Rules and Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





% other_player(+CurrentPlayer,-NextPlayer)
% Change player turn
other_player(pl1, pl2).
other_player(pl2, pl1).

% valid_get_move_coordinate(+Value,-Max)
% Checks if the entered value_of_board is between 1 and the maximum
valid_get_move_coordinate(Value, Max) :-
    integer(Value),
    Value >= 1,
    Value =< Max.

% set_neutral_pawn_coordinate(-Coordinate)
set_neutral_pawn_coordinate(Row-Column) :-
    retractall(neutral_pawn_coordinates(_)),
    asserta(neutral_pawn_coordinates(Row-Column)).

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
    retractall(player_symbol(_,_)),
    asserta(player_symbol(pl1, W)),
    asserta(player_symbol(pl2, B)).

% is_cell(+Board, -Row, -Col)
% Checks whether the position with the respective coordinates is a cell
is_cell_empty(Board, Row-Column) :-
    nth1(Row, Board, RowList), 
    nth1(Col, RowList, cell).

% is_not_obstructed_and_valid(+Board, +Row, +Col)
% Checks if there are no pieces in the way of moving and if it is on the board and it is a possible move
is_not_obstructed_and_valid(Board, Row, Col) :-
    length(Board, Rows),
    board(Size,_,Rows),
    neutral_pawn_coordinates(NeutralRow-NeutralCol),
    moves_from_neutral(Size, NeutralRow-NeutralCol, PathList),
    memberchk(Row-Column, PathList), !,
    not_obstructed(Board, Row-Column, PathList).

% not_obstructed(+Board, +Row-Column, +PathList)
% Checks if there are any pieces in the way of the move from the neutral pawn
not_obstructed(_, Row-Column, [Row-Column|_]).
not_obstructed(Board, Row-Column, [H|T]) :-
    is_cell_empty(Board, H),
    not_obstructed(Board, Row-Column, T).

% place_neutral(+Board, +Row-Column, -NewBoard)
% Places the neutral pawn at the given row and column
place_neutral(Board, Row-Column, NewBoard) :-
    RowIndex is Row - 1, ColIndex is Col - 1,
    nth0(RowIndex,Board,Line),
    replace_element(ColIndex, n, Line, NewLine),
    replace_element(RowIndex, NewLine, Board, NewBoard),
    set_neutral_pawn_coordinate(Row-Column).

% replace_neutral_pawn(+Board, +NeutralRow-NeutralCol, +Checker, -NewBoard)
% Replaces the neutral pawn at the position NeutralRow-NeutralCol with the provided Checker
replace_neutral_pawn(Board, NeutralRow-NeutralCol, Checker, NewBoard):-
    RowIndex is NeutralRow - 1, ColIndex is NeutralCol - 1,
    nth0(RowIndex,Board,Line),
    replace_element(ColIndex, Checker, Line, NewLine),
    replace_element(RowIndex, NewLine, Board, NewBoard). 

% replace_element(+Index,+Element,+List,-Result)
% Replace an element at Index in List by Element to get Result.
replace_element(Index, Element, List, Result) :-
  nth0(Index, List, _, R),
  nth0(Index, Result, Element, R).

% at_least_one_notused(+Board, +Size, +NeutralRow-NeutralCol)
% Checks if there exists at least one notused in the board
at_least_one_notused(Board, Size, NeutralRow-NeutralCol) :-
    moves_from_neutral(Size, NeutralRow-NeutralCol, List),
    nth1(1, List, FirstElement),
    is_cell_empty(Board, FirstElement), !.

% total_score(+Board, +Size, +NeutralRow-NeutralCol, +Player, +OtherPlayer)
% Determines the total score
total_score(Board, Size, NeutralRow-NeutralCol, Player, OtherPlayer) :-
    list_of_first_elements(Size, NeutralRow-NeutralCol, FirstElementsList),
    points_due_to_checker(Board, Player, OtherPlayer, FirstElementsList).

% list_of_first_elements(+Size, +NeutralRow-NeutralCol, -FirstElementsList)
% Returns a list containing all first elements of possible paths from the neutral pawn
list_of_first_elements(Size, NeutralRow-NeutralCol, FirstElementsList) :-
    bagof(FirstElement, Args^(moves_from_neutral(Size, NeutralRow-NeutralCol, Args), nth1(1, Args, FirstElement)), FirstElementsList).

% points_due_to_checker(+Board, +Player, +OtherPlayer, +List)
% Add points due to checker type
points_due_to_checker(_,_,_,[]).
points_due_to_checker(Board, Player, OtherPlayer, [H|T]) :-
    checker_type(Board, H, Type),
    based_on_type_add_score(Type, Player, OtherPlayer),
    points_due_to_checker(Board, Player, OtherPlayer, T).

% checker_type(+Board, +Row-Column, -Type)
% Find out what kind of checker it is at the specified Row and Column
checker_type(Board, Row-Column, Type) :-
    nth1(Row, Board, RowList), 
    nth1(Col, RowList, Type).

% based_on_type_add_score(+Type, +Player, +OtherPlayer)
% Based on the type of checker, add appropriate scores for player or otherplayer
based_on_type_add_score(Type, Player, OtherPlayer) :-
    player_symbol(Player, Checker),
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



% is_possible_win(+GameState, +Player, +OtherPlayer, +List, -Row-Column, -Return)
% Checks the possibility of winning based on a list of coordinates of valid moves                                                                          
is_possible_win(_,_,_,[],_,_).
is_possible_win([Board, Player, _], Player, OtherPlayer, [H|T], Row-Column, Return) :-
    length(Board, Rows),
    board(Size, _, Rows),
    at_least_one_notused(Board, Size, H),
    Return = continue.
is_possible_win([Board, Player, _], Player, OtherPlayer, [H|T], Row-Column, Return) :-
    length(Board, Rows),
    board(Size, _, Rows),
    \+ at_least_one_notused(Board, Size, H),
    asserta(player_score(Player,1)),
    preview_score(Board, Size, H, Player, OtherPlayer),
    player_score(Player, Score),
    Score > 0 ->
        Row-Column = H,
        Return = break
    ;
    is_possible_win([Board, Player, _], Player, OtherPlayer, T, Row-Column, Return).

% preview_score(+Board, +Size, +NeutralRow-NeutralCol, +Player, +OtherPlayer)
% Calculates the score based on the game board and a specific coordinate
preview_score(Board, Size, NeutralRow-NeutralCol, Player, OtherPlayer) :-
    list_of_first_elements(Size, NeutralRow-NeutralCol, FirstElementsList),
    add_or_subtract_based_on_checker_type(Board, Player, OtherPlayer, FirstElementsList).

% add_or_subtract_based_on_checker_type(+Board, +Player, +OtherPlayer, +List)
% Adds or subtracts score based on the checker type encountered in the provided list
add_or_subtract_based_on_checker_type(_,_,_,[]).
add_or_subtract_based_on_checker_type(Board, Player, OtherPlayer, [H|T]) :-
    checker_type(Board, H, Type),
    add_or_sub_score_based_on_type(Type, Player),
    add_or_subtract_based_on_checker_type(Board, Player, OtherPlayer, T).

% add_or_sub_score_based_on_type(+Type, +Player)
% Adds or subtracts score based on the type of checker found
add_or_sub_score_based_on_type(Type, Player) :-
    player_symbol(Player, Checker),
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

% size_of_list_of_moves(+ListOfMoves, +Size)
% Returns the number of moves that can be made
size_of_list_of_moves(ListOfMoves, Size):-
    length(LisOfMoves, Size).