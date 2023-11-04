:- use_module(library(between)).
:- use_module(library(lists)).
:- use_module(library(random)).

% clear_data/0
% Eliminates predicates from the programs knowledge base
clear_data :-
    retractall(player(_, _)),
    retractall(player_checker(_,_)),
    retractall(neutral_pawn_coordinates(_)),
    retractall(player_score(_,_)).
    retractall(difficulty(_, _)),
    retractall(name_of(_, _)).