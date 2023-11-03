:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(system), [now/1]).
:- consult(utils).
:- consult(data).

% choose_difficulty(+Bot)
% Choose Bot difficulty (1 or 2)
choose_difficulty(Bot) :-
    format('Please select ~a status:\n', [Bot]),
    write('1 - Random\n'),
    write('2 - Greedy\n'),
    get_option(1, 2, 'Difficulty', Option), !,
    asserta((difficulty(Bot, Option))).

% option(+N)
% Main menu options. Each represents a game mode.
option(1):-
    write('Human vs. Human\n'),
    get_name(player1), get_name(player2).
option(2):-
    write('Human vs. Bot\n'),
    get_name(player1),
    asserta((name_of(player2, 'bot'))), !, 
    choose_difficulty(player2).
option(3):-
    write('Bot vs. Bot\n'),
    asserta((name_of(player1, 'bot1'))),
    asserta((name_of(player2, 'bot2'))), !,
    choose_difficulty(player1),
    choose_difficulty(player2).

% choose_player(-Player)
% Unifies player with the player who will start the game
choose_player(Player):-
    name_of(player1, Name1),
    name_of(player2, Name2),
    format('Who starts playing?\n1 - ~a with White\n2 - ~a with Black\n', [Name1, Name2]),
    get_option(1, 2, 'Select', Index),
    nth1(Index, [player1, player2], Player).

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
    write('2 - Human vs. Bot\n'),
    write('3 - Bot vs. Bot\n').

% set_mode/0
% Game mode choice
set_mode :-
    menu,
    get_option(1, 3, 'Mode', Option), !,
    option(Option).

% choose_board(-Size)
% Escolha do tamanho do tabuleiro
choose_board(Size):-
    write('Escolha o tamanho do tabuleiro:\n'),
    write('1 - Small\n'),
    write('2 - Medium\n'),
    write('3 - Large\n'),
    repeat,
    read_number(Choice),
    (
        (Choice = 1, Size = 13);
        (Choice = 2, Size = 17);
        (Choice = 3, Size = 21)
    ),
    !.


% configuration(-GameState)
% Initialize GameState with Board, first Player, empty FearList and TotalMoves
configurations([Board,Player,[],0]):-
    Trike,
    set_mode,
    init_random_state,
    choose_player(Player),
    choose_board(Size), 
    init_state(Size, Board).