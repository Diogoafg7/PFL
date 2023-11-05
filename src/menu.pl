:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(system), [now/1]).
:- consult(utils).
:- consult(board).

% Trike/0
% Game header
Trike:-
    write('===========================\n'),
    write('|                         |\n'),
    write('|         T R I K E       |\n'),
    write('|                         |\n'),
    write('===========================\n').


% menu/0
% Main menu
menu:-  
    write('Please select game mode:\n'),
    write('1 - Human vs. Human\n'),
    write('2 - Human vs. Computer\n'),
    write('3 - Computer vs. Computer\n'),
    write('0 - Exit\n').

% menu_option(+Input)
% Based on the user input, executes a specific action corresponding to the chosen game mode or handles an invalid input.
menu_option(0) :-
    write('\nEnding the game. Thank you for playing Trike\n\n').

menu_option(1) :-
    write('Human vs Human\n'),
    write('Enter the name for Player 1: '),
    read(Player1Name),
    asserta(player(player1, Player1Name)),
    write('Enter the name for Player 2: '),
    read(Player2Name),
    asserta(player(player2, Player2Name)).

menu_option(2) :-
    write('Human vs Computer\n'),
    write('Enter your name: '),
    read(Player1Name),
    asserta(player(player1, Player1Name)),
    asserta(player(player2, 'Computer')), 
    computer_difficulty_level(player2).

menu_option(3) :-
    write('Computer vs Computer\n'),
    write('Enter the name for Computer 1: '),
    read(Player1Name),
    write('Enter the name for Computer 2: '),
    read(Player2Name),
    asserta(player(player1, Player1Name)),
    asserta(player(player2, Player2Name)),
    computer_difficulty_level(player1),
    computer_difficulty_level(player2).

menu_option(_Other) :-
    write('\nERROR: Invalid option!\n\n'),
    write('Enter one of the options (e.g. "1." for Player vs Player game mode): '),
    read(Input),
    menu_option(Input).

% first_move_player(-Player)
% Allows the user to choose or selects the player who makes the first move randomly.
first_move_player(Player) :-
    write('Choose the player who will make the first move:\n'),
    write('1 - Player 1\n'),
    write('2 - Player 2\n'),
    write('3 - Random\n'),
    read(UserChoice),
    (
        UserChoice = 1 -> Player = player1, write('Player 1 will make the first move.\n');
        UserChoice = 2 -> Player = player2, write('Player 2 will make the first move.\n');
        UserChoice = 3 -> random_member(Player, [player1, player2]), format('%w will make the first move.\n', [Player]);
        write('Invalid choice. Please select 1, 2, or 3.\n'),
        first_move_player(Player)
    ).

% board_size(-Size)
% Reads the chosen size for the game board from the user input.
board_size(Size) :-
    write('Choose the size for the game board:\n'),
    write('1 - Small (7)\n'),
    write('2 - Medium (13)\n'),
    write('3 - Large (17)\n'),
    read(UserChoice),
    (
        UserChoice = 1 -> Size = 6, write('You selected a small board (6).\n');
        UserChoice = 2 -> Size = 15, write('You selected a medium board (15).\n');
        UserChoice = 3 -> Size = 28, write('You selected a large board (28).\n');
        write('Invalid choice. Please select 1, 2, or 3.\n'),
        board_size(Size)
    ).

% computer_difficulty_level(+Computer)
% Enables the user to choose the difficulty level for the specified Computer
computer_difficulty_level(Computer) :-
    write('Choose the difficulty level for the computer player:\n'),
    write('1 - Easy (Random Movement)\n'),
    write('2 - Medium (Greedy Movement)\n'),
    read(UserChoice),
    (
        UserChoice = 1 -> Level = 1, write('You selected Easy (Random Movement).\n');
        UserChoice = 2 -> Level = 2, write('You selected Medium (Greedy Movement).\n');
        write('Invalid choice. Please select 1 or 2 for the difficulty level.\n'),
        computer_difficulty_level(Computer)
    ),
    asserta(difficulty_level(Computer, Level)).

% player symbol
default_player_checker :-
    asserta(player_checker(player1, W)),
    asserta(player_checker(player2, B)).

set_default_neutral_pawn_coordinates(Size) :-
    board(Size, Cols, Rows),
    asserta(neutral_pawn_coordinates(Rows-Cols)).

% game_setup(-GameState)
% Prompts the user to select a game mode, handles the chosen mode, chooses the player who makes the first move,
% asks for the board size, and initializes the board state.
% Initializes the game state with the Board and the player who makes the first move
game_setup([Board, Player, MoveNumber]) :-
    Trike,
    menu,
    read(Input),
    menu_option(Input),
    first_move_player(Player),
    board_size(Size),
    default_player_checker,
    set_default_neutral_pawn_coordinates(Size),
    MoveNumber is 1,
    initial_state(Size, [Board,_,_]), !.
