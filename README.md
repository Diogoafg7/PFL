# PFL
# TRIKE
Grupo Trike_7:

| Grupo | Nome                         | UP            | Contribuição |
|------------   | ------------                 | ------------  |------------  |
|Trike 7| Diogo Alexandre Figueiredo Gomes| [up201905991] |50%           |
|| Rafael Ferreira da Costa Silva Valquaresma     | [up202104805] |50%           |


## Instruções de Instalação do Jogo Trike

Para instalar o jogo Trike, siga as etapas abaixo:

1. Faça o download dos arquivos contidos no arquivo [PFL_Trike7.zip] e descompacte-os em um diretório de sua escolha.

2. Navegue até a pasta "src" no diretório onde os arquivos foram descompactados.

3. Abra o arquivo "trike.pl" usando o Sicstus Prolog 4.8. Isso pode ser feito através da linha de comando ou pela interface gráfica do Sicstus Prolog.

4. Inicie o jogo executando o predicado "play/0" no Sicstus Prolog.

## Descrição do Jogo

Trike é um jogo de estratégia abstrata e combinatória projetado para dois jogadores. A dinâmica do jogo gira em torno da criação de armadilhas, da desmontagem das armadilhas do adversário e da manobra de uma peça compartilhada, conhecida como peão neutro, com o objetivo de alcançar a vitória.

### Regras Principais:

- **Tabuleiro**: O tabuleiro do jogo possui a forma de um triângulo equilátero.

- **Peças Utilizadas**: No Trike, são utilizados um peão neutro, bem como peças de cores preta e branca para representar os jogadores.

- **Fase Inicial**: No início da partida, o primeiro jogador posiciona uma peça em qualquer local do tabuleiro e coloca o peão neutro sobre ela. Neste momento, o segundo jogador tem a oportunidade única de trocar de lado (mudar de peça) em vez de efetuar um movimento convencional.

- **Movimento do Peão Neutro**: O peão neutro tem a capacidade de mover-se por qualquer número de posições vazias, seguindo uma trajetória em linha reta, em qualquer direção. No entanto, ele não pode mover-se através de posições ocupadas por outras peças.

- **Colocação de Peças**: Quando um jogador coloca uma peça em uma posição permitida, o peão neutro é transferido para essa peça, ocupando-a.

- **Término do Jogo**: O jogo encerra quando o peão neutro fica impossibilitado de realizar movimentos.

- **Contagem de Pontos**: Ao final da partida, cada jogador acumula um ponto por cada peça de sua cor que estiver adjacente ao peão neutro ou posicionada abaixo dele. O jogador com a maior pontuação é declarado o vencedor. Empates não são possíveis.

### Nota Importante:

Essas são as regras básicas do Trike. Para informações detalhadas e estratégias avançadas, consulte o [website oficial do jogo](https://boardgamegeek.com/boardgame/307379/trike).
