:- dynamic tabuleiro/2.
:- dynamic jogador_atual/1.

% Definir os jogadores
jogador(preto).
jogador(branco).
jogador(computador).

% Modos de Jogo
modo_de_jogo(pvp).
modo_de_jogo(pvc).
modo_de_jogo(cvc).

% Tamanhos de Tabuleiro
tamanho_do_tabuleiro(5). % Tamanho padrão do tabuleiro

% Níveis de Dificuldade para o Computador
dificuldade(computador, facil).
dificuldade(computador, medio).
dificuldade(computador, dificil).

% Iniciar o jogo
iniciar_jogo(ModoJogo, TamanhoTabuleiro, Dificuldade) :-
    assert(tabuleiro([], TamanhoTabuleiro)), % Inicializar tabuleiro vazio
    assert(jogador_atual(preto)), % O jogador preto começa
    menu_jogo(ModoJogo, TamanhoTabuleiro, Dificuldade).

% Menu do Jogo
menu_jogo(ModoJogo, TamanhoTabuleiro, Dificuldade) :-
    write('\n** Trike - Menu **\n'),
    write('1. Iniciar Jogo\n'),
    write('2. Escolher Modo de Jogo\n'),
    write('3. Escolher Tamanho de Tabuleiro\n'),
    write('4. Escolher Dificuldade do Computador\n'),
    write('5. Sair do Jogo\n'),
    write('Escolha uma opção: '),
    read(Opcao),
    escolher_opcao(Opcao, ModoJogo, TamanhoTabuleiro, Dificuldade).

% Lidar com a escolha do jogador
escolher_opcao(1, ModoJogo, TamanhoTabuleiro, Dificuldade) :-
    iniciar_partida(ModoJogo, TamanhoTabuleiro, Dificuldade).
escolher_opcao(2, ModoJogo, TamanhoTabuleiro, Dificuldade) :-
    escolher_modo_jogo(NovoModo),
    menu_jogo(NovoModo, TamanhoTabuleiro, Dificuldade).
escolher_opcao(3, ModoJogo, TamanhoTabuleiro, Dificuldade) :-
    escolher_tamanho_tabuleiro(NovoTamanho),
    menu_jogo(ModoJogo, NovoTamanho, Dificuldade).
escolher_opcao(4, ModoJogo, TamanhoTabuleiro, Dificuldade) :-
    escolher_dificuldade(NovaDificuldade),
    menu_jogo(ModoJogo, TamanhoTabuleiro, NovaDificuldade).
escolher_opcao(5, _, _, _) :-
    write('Obrigado por jogar! Até a próxima.\n'),
    halt.
escolher_opcao(_, ModoJogo, TamanhoTabuleiro, Dificuldade) :-
    write('Opção inválida. Tente novamente.\n'),
    menu_jogo(ModoJogo, TamanhoTabuleiro, Dificuldade).

% Outras opções do menu (escolher modo de jogo, tamanho do tabuleiro, dificuldade)
escolher_modo_jogo(NovoModo) :-
    write('Escolha o Modo de Jogo:\n'),
    write('1. Player vs Player (pvp)\n'),
    write('2. Player vs Computer (pvc)\n'),
    write('3. Computer vs Computer (cvc)\n'),
    write('Opção: '),
    read(Opcao),
    (
        Opcao = 1 -> NovoModo = pvp;
        Opcao = 2 -> NovoModo = pvc;
        Opcao = 3 -> NovoModo = cvc
    ).

escolher_tamanho_tabuleiro(NovoTamanho) :-
    write('Escolha o Tamanho do Tabuleiro:\n'),
    write('1. 5x5\n'),
    write('2. 6x6\n'),
    write('3. 7x7\n'),
    write('Opção: '),
    read(Opcao),
    (
        Opcao = 1 -> NovoTamanho = 5;
        Opcao = 2 -> NovoTamanho = 6;
        Opcao = 3 -> NovoTamanho = 7
    ).

escolher_dificuldade(NovaDificuldade) :-
    write('Escolha a Dificuldade do Computador:\n'),
    write('1. Fácil\n'),
    write('2. Médio\n'),
    write('3. Difícil\n'),
    write('Opção: '),
    read(Opcao),
    (
        Opcao = 1 -> NovaDificuldade = facil;
        Opcao = 2 -> NovaDificuldade = medio;
        Opcao = 3 -> NovaDificuldade = dificil
    ).

% Iniciar a partida com base nas escolhas feitas
iniciar_partida(ModoJogo, TamanhoTabuleiro, Dificuldade) :-
    retractall(tabuleiro(_, _)), % Remover o tabuleiro anterior
    assert(tabuleiro([], TamanhoTabuleiro)), % Inicializar tabuleiro vazio
    assert(jogador_atual(preto)), % O jogador preto começa
    jogar_partida(ModoJogo, Dificuldade).

% Outras regras e lógica do jogo aqui...

% Exemplo de uso:
% ?- iniciar_jogo(pvp, 5, facil).
