:- use_module(library(between)).
:- use_module(library(lists)).
:- use_module(library(random)).

% clear_data/0
% Eliminates predicates from the programs knowledge base
clear_data :-
    retractall(player(_, _)),
    retractall(player_symbol(_,_)),
    retractall(difficulty_level(_, _)),
    retractall(neutral_pawn_coordinates(_)),
    retractall(player_score(_,_)).

% clear_console/0
% Clears console
clear_console:- 
    write('\33\[2J').
