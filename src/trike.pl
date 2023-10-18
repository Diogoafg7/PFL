% Estruturas de Dados
% Estrutura de dados para representar as cores das peças.
piece(white).
piece(black).

% Estrutura de dados para representar o estado do jogo.
% game(Board, PawnPos, Player, WhiteScore, BlackScore)
% Board: Tabuleiro representado por uma lista de listas.
% PawnPos: Posição atual do pino neutro [X, Y].
% Player: Jogador atual (white ou black).
% WhiteScore: Pontuação do jogador branco.
% BlackScore: Pontuação do jogador preto.
game(Board, [X, Y], Player, WhiteScore, BlackScore).

% Configuração Inicial
initialize_game(game(Board, [14, 14], white, 0, 0)):-
    create_empty_board(28, Board).

% Inicializa o tabuleiro com um tamanho especificado, preenchendo-o com peças vazias.
create_empty_board(Size, Board):-
    create_empty_board(Size, Size, [], Board).

create_empty_board(0, _, Board, Board).
create_empty_board(Rows, Size, PartialBoard, Board):-
    create_row(Size, Row),
    NewRows is Rows - 1,
    create_empty_board(NewRows, Size, [Row | PartialBoard], Board).

create_row(0, []).
create_row(Size, [empty | Rest]):-
    NewSize is Size - 1,
    create_row(NewSize, Rest).

% Movimento do Pino Neutro
move_pawn(game(Board, [X, Y], Player, WhiteScore, BlackScore), NewGame):-
    % Verifica se o movimento é válido (espaço vazio e direção desobstruída).
    valid_move(X, Y, NewX, NewY, Board),
    % Atualiza o tabuleiro com a nova posição do pino neutro.
    update_board(Board, X, Y, NewX, NewY, UpdatedBoard),
    % Atualiza o estado do jogo com a nova posição e o próximo jogador.
    switch_player(Player, NextPlayer),
    NewGame = game(UpdatedBoard, [NewX, NewY], NextPlayer, WhiteScore, BlackScore).

% Verifica se o movimento é válido.
valid_move(X, Y, NewX, NewY, Board):-
    % Verifica se as coordenadas de destino estão dentro dos limites do tabuleiro.
    valid_coordinates(NewX, NewY),
    % Obtém o conteúdo do espaço de destino.
    get_cell(Board, NewX, NewY, Cell),
    % Verifica se o espaço de destino está vazio.
    Cell = empty,
    % Verifica se a direção está desobstruída.
    is_direction_clear(X, Y, NewX, NewY, Board).

% Verifica se as coordenadas estão dentro dos limites do tabuleiro.
valid_coordinates(X, Y):-
    X >= 1,
    X =< 28,
    Y >= 1,
    Y =< 28.

% Obtém o conteúdo de uma célula no tabuleiro.
get_cell(Board, X, Y, Cell):-
    nth1(Y, Board, Row),
    nth1(X, Row, Cell).

% Verifica se a direção está desobstruída.
is_direction_clear(X, Y, NewX, NewY, Board):-
    % Verifica se as coordenadas são horizontais, verticais ou diagonais.
    (abs(NewX - X) =:= abs(NewY - Y) ; NewX =:= X ; NewY =:= Y),
    % Chama um predicado auxiliar para verificar as células intermediárias.
    is_direction_clear_aux(X, Y, NewX, NewY, Board).

% Predicado auxiliar para verificar as células intermediárias.
is_direction_clear_aux(X, Y, NewX, NewY, Board):-
    % Calcula os passos entre as coordenadas atuais e as coordenadas de destino.
    StepX is sign(NewX - X),
    StepY is sign(NewY - Y),
    % Calcula as coordenadas da próxima célula na direção.
    NextX is X + StepX,
    NextY is Y + StepY,
    % Obtém o conteúdo da próxima célula.
    get_cell(Board, NextX, NextY, Cell),
    % Verifica se a próxima célula está vazia (ou seja, sem peças).
    Cell = empty,
    % Recursivamente verifica a próxima célula.
    is_direction_clear_aux(NextX, NextY, NewX, NewY, Board).

%///////////////////////////////////////////////////////////CONTINUAR AQUI///////////////////7

% Atualiza o tabuleiro com a nova posição do pino neutro.
update_board(Board, X, Y, NewX, NewY, UpdatedBoard):-
    % Implemente a lógica para atualizar o tabuleiro.

% Colocação de Cubos
place_cube(game(Board, [X, Y], Player, WhiteScore, BlackScore), NewGame):-
    % Verifica se o espaço de destino está vazio.
    valid_placement(X, Y, Board),
    % Atualiza o tabuleiro com a peça do jogador.
    place_piece(Board, X, Y, Player, UpdatedBoard),
    % Atualiza a pontuação do jogador.
    update_score(Player, WhiteScore, BlackScore, NewWhiteScore, NewBlackScore),
    % Verifica a condição de vitória e determina o vencedor, se aplicável.
    game_over(game(UpdatedBoard, [X, Y], Player, NewWhiteScore, NewBlackScore), Winner),
    NewGame = game(UpdatedBoard, [X, Y], Winner, NewWhiteScore, NewBlackScore).

% Verifica se o espaço de destino está vazio.
valid_placement(X, Y, Board):-
    % Implemente a lógica para verificar se o espaço de destino está vazio.

% Atualiza o tabuleiro com a peça do jogador.
place_piece(Board, X, Y, Player, UpdatedBoard):-
    % Implemente a lógica para atualizar o tabuleiro com a peça do jogador.

% Atualiza a pontuação do jogador.
update_score(white, WhiteScore, BlackScore, NewWhiteScore, BlackScore):-
    NewWhiteScore is WhiteScore + 1.
update_score(black, WhiteScore, BlackScore, WhiteScore, NewBlackScore):-
    NewBlackScore is BlackScore + 1.

% Verifica a condição de vitória e determina o vencedor.
game_over(game(Board, _, _, WhiteScore, BlackScore), Winner):-
    % Implemente a lógica para verificar se o jogo terminou e determinar o vencedor.

% Alternância de Jogadores
switch_player(white, black).
switch_player(black, white).
