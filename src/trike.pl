consult('menu.pl').

% Estruturas de Dados
% Estrutura de dados para representar as cores das peças.
piece(white).
piece(black).

% Estrutura de dados para representar a posição de uma célula no tabuleiro triangular.
cell(X, Y).

% Estrutura de dados para representar os modos de jogo.
game_mode(pvp).  % Player vs. Player
game_mode(pvc).  % Player vs. Computador
game_mode(cvc).  % Computador vs. Computador


% Estrutura de dados para representar o estado do jogo.
% game(Board, PawnPos, Player, WhiteScore, BlackScore)
% Board: Tabuleiro representado por uma lista de células.
% PawnPos: Posição atual do pino neutro cell(X, Y).
% Player: Jogador atual (white ou black).
% WhiteScore: Pontuação do jogador branco.
% BlackScore: Pontuação do jogador preto.
game(Board, cell(X, Y), Player, WhiteScore, BlackScore).

% Configuração Inicial
initialize_game(game(Board, cell(14, 14), white, 0, 0)):-
    create_empty_board(14, 14, Board).

% Inicializa o tabuleiro com um tamanho especificado.
create_empty_board(0, _, []).
create_empty_board(Size, Rows, [cell(Size, Rows) | Rest]) :-
    NextRows is Rows - 1,
    create_empty_board(Size, NextRows, Rest).

% Movimento do Pino Neutro
move_pawn(game(Board, cell(X, Y), Player, WhiteScore, BlackScore), NewGame):-
    % Verifica se o movimento é válido (espaço vazio e direção desobstruída).
    valid_move(X, Y, NewX, NewY, Board),
    % Atualiza o tabuleiro com a nova posição do pino neutro.
    NewGame = game(Board, cell(NewX, NewY), Player, WhiteScore, BlackScore).

% Verifica se o movimento é válido.
valid_move(X, Y, NewX, NewY, Board):-
    % Calcula as coordenadas da próxima célula na direção.
    adjacent_position(X, Y, NewX, NewY),
    % Verifica se a célula de destino está vazia.
    \+ occupied_cell(NewX, NewY, Board).

% Verifica se a célula está ocupada.
occupied_cell(X, Y, Board):-
    member(cell(X, Y), Board).

% Define as posições adjacentes a uma célula no tabuleiro triangular.
adjacent_position(X, Y, NewX, NewY):-
    % Possui 6 vizinhos adjacentes (hexagonais).
    (
        (NewX is X - 1, NewY is Y);
        (NewX is X + 1, NewY is Y);
        (NewX is X, NewY is Y - 1);
        (NewX is X, NewY is Y + 1);
        (NewX is X - 1, NewY is Y - 1);
        (NewX is X + 1, NewY is Y + 1)
    ).

% Restante do seu código permanece inalterado.

% Colocação de Cubos
place_cube(game(Board, cell(X, Y), Player, WhiteScore, BlackScore), NewGame):-
    % Verifica se o espaço de destino está vazio.
    valid_placement(X, Y, Board),
    % Atualiza o tabuleiro com a peça do jogador.
    place_piece(Board, X, Y, Player, UpdatedBoard),
    % Atualiza a pontuação do jogador.
    update_score(Player, WhiteScore, BlackScore, NewWhiteScore, NewBlackScore),
    % Verifica a condição de vitória e determina o vencedor, se aplicável.
    game_over(game(UpdatedBoard, cell(X, Y), Player, NewWhiteScore, NewBlackScore), Winner),
    NewGame = game(UpdatedBoard, cell(X, Y), Winner, NewWhiteScore, NewBlackScore).

% Predicado para verificar se a célula está vazia.
empty_cell(Board, X, Y):-
    in_bounds(X, Y, Board),  % Verifica se as coordenadas estão dentro dos limites do tabuleiro.
    \+ occupied_cell(X, Y, Board).  % Verifica se a célula está vazia.

% Verifica se o espaço de destino está vazio.
valid_placement(X, Y, Board):-
    empty_cell(Board, X, Y).

% Predicado para atualizar o tabuleiro com a peça do jogador.
place_piece(Board, X, Y, Player, NewBoard):-
    assert(occupied_cell(X, Y, Board)),
    update_score(Player, WhiteScore, BlackScore, NewWhiteScore, NewBlackScore),
    retract(occupied_cell(X, Y, Board)),
    NewBoard = [cell(X, Y) | Board].

% Atualiza a pontuação do jogador.
update_score(white, WhiteScore, BlackScore, NewWhiteScore, BlackScore):-
    NewWhiteScore is WhiteScore + 1.
update_score(black, WhiteScore, BlackScore, WhiteScore, NewBlackScore):-
    NewBlackScore is BlackScore + 1.

% Predicado para verificar a condição de vitória e determinar o vencedor.
game_over(game(Board, cell(X, Y), _, WhiteScore, BlackScore), Winner):-
    % Verifica se o pino neutro não pode mais se mover.
    \+ can_move(X, Y, Board),
    % Conta o número de peças brancas e pretas adjacentes ao pino neutro.
    count_adjacent(X, Y, Board, white, WhiteAdjacent),
    count_adjacent(X, Y, Board, black, BlackAdjacent),
    % Compara o número de peças brancas e pretas adjacentes para determinar o vencedor.
    (
        WhiteAdjacent > BlackAdjacent, Winner = white;
        BlackAdjacent > WhiteAdjacent, Winner = black;
        % Em caso de empate, não há vencedor.
        Winner = draw
    ).

% Verifica se o pino neutro pode se mover em alguma direção.
can_move(X, Y, Board):-
    % Verifica se a posição acima do pino está vazia.
    NewX is X - 1, NewY is Y, empty_cell(Board, NewX, NewY), !;
    % Verifica se a posição abaixo do pino está vazia.
    NewX is X + 1, NewY is Y, empty_cell(Board, NewX, NewY), !;
    % Verifica se a posição à esquerda do pino está vazia.
    NewX is X, NewY is Y - 1, empty_cell(Board, NewX, NewY), !;
    % Verifica se a posição à direita do pino está vazia.
    NewX is X, NewY is Y + 1, empty_cell(Board, NewX, NewY), !;
    % Verifica se a posição acima à esquerda do pino está vazia.
    NewX is X - 1, NewY is Y - 1, empty_cell(Board, NewX, NewY), !;
    % Verifica se a posição abaixo à direita do pino está vazia.
    NewX is X + 1, NewY is Y + 1, empty_cell(Board, NewX, NewY), !;
    fail.

% Obtém a peça na posição [X, Y] do tabuleiro.
get_piece(Board, X, Y, Piece):-
    (member(cell(X, Y), Board), Piece = Player), !;
    Piece = empty.

% Conta o número de peças adjacentes da cor especificada.
count_adjacent(X, Y, Board, Color, Count):-
    findall(_, (adjacent_position(X, Y, AdjX, AdjY), get_piece(Board, AdjX, AdjY, Color)), List),
    length(List, Count).

% Define as posições adjacentes ao pino neutro.
adjacent_position(X, Y, X1, Y):-
    X1 is X - 1, X1 > 0.
adjacent_position(X, Y, X2, Y):-
    X2 is X + 1, X2 =< 14.
adjacent_position(X, Y, X, Y1):-
    Y1 is Y - 1, Y1 > 0.
adjacent_position(X, Y, X, Y2):-
    Y2 is Y + 1, Y2 =< 14.
adjacent_position(X, Y, X3, Y3):-
    X3 is X - 1, Y3 is Y - 1, X3 > 0, Y3 > 0.
adjacent_position(X, Y, X4, Y4):-
    X4 is X + 1, Y4 is Y + 1, X4 =< 14, Y4 =< 14.

% Alternância de Jogadores
switch_player(white, black).
switch_player(black, white).

