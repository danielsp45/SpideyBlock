{- |
Module      : Tarefa3_2021li1g040
Description : Representação textual do jogo
Copyright   : Daniel da Silva Pereira <a100545@alunos.uminho.pt>;
            : Rodrigo Viana Ramos Casal Novo <a100534@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.

= Visualização Gráfica do Jogo

A visualização de um jogo é feita através do quatro passos, se o jogador carrega uma caixa, ou três, se tal não acontecer.

Caso o jogador não tenha uma caixa, a função corre a primeira linha numa auxiliar e, com a ajuda de um acumulador,
verifica se é a coordenada onde colocar o jogador, quando a coordenada atual não é a do jogador utiliza a função 'showPeca',
se,por outro lado, forem as coordenadas do jogador, ao invês da peça, é colocado o jogador com a função 'showJogador'. ('insereJogador')

Quando acabar a linha, independentemente do jogador, é puxado outra vez a função para a linha a seguir (separando-as com '\n').

Caso o jogador tenha uma caixa, é necessário colocar essa caixa no mapa. Para tal é utilizado uma função que compara a coordenada atual (disponiblizada por um acomulador) com a coordenada da caixa(coluna do jogador e linha a cima),
se forem iguais coloca a caixa e muda o jogador para false correndo então a função anterior, se for diferente escreve a peça daquela posição com o 'showPeca' e avança para a coordenada a seguir.('insereCaixa')

-}

module Tarefa3_2021li1g040 where

import LI12122

instance Show Jogo where
  show (Jogo  m j) = showJogo m j (0,0)

-- | função auxiliar do instance 

showJogo :: Mapa -- ^ Recebe o mapa do jogo   
         -> Jogador -- ^ Recebe o jogador 
         -> Coordenadas -- ^ Recebe a origem do mapa (0,0) como acumulador
         -> String -- ^ Devolve o mapa com as strings das linhas separadas por '\n'  
showJogo [] _ _ = []
showJogo [pecas] j (c,l) = insereJogador j pecas (c,l)
showJogo (pecas: resto) j@(Jogador (cj,lj) d True) (c,l) |(lj-1) == l = insereCaixa pecas (cj,lj-1) (c,l) ++ "\n" ++ showJogo resto (Jogador (cj,lj) d False) (c,l+1)
                                                         | otherwise = insereJogador j pecas (c,l) ++ "\n" ++ showJogo resto j (c,l+1)
showJogo (pecas: resto) j@(Jogador (cj,lj) d False) (c,l) =  insereJogador j pecas (c,l) ++ "\n" ++ showJogo resto j (c,l+1)

-- | insere jogador no mapa
insereJogador :: Jogador -- ^ Recebe o Jogador sem caixa 
              -> [Peca] -- ^  Recebe uma linha
              -> Coordenadas -- ^  Usa um acomulador com a coluna e linha atuais
              -> String -- ^  Devolve a String de uma linha 
insereJogador j [] _= ""
insereJogador j@(Jogador (c,l) d b) (p:pcs) (c1,l1) | (c == c1) && (l == l1) && (p == Vazio) = showJogador j ++ insereJogador j pcs (c1+1,l1)
                                                    | otherwise = showPeca p ++ insereJogador j pcs (c1+1,l1)
{-| 
== Representação das Peças
-}
insereCaixa :: [Peca] -- ^ Recebe uma linha 
            -> Coordenadas -- ^  Recebe as coordenadas onde colocar caixa
            -> Coordenadas -- ^  Usa o mesmo acomulador com a coluna e linha atuais
            -> String -- ^  Devolve a String de uma linha
insereCaixa  [] _ _= ""
insereCaixa (p:pcs) (cc,lc) (c1,l1) | (cc == c1) && (lc == l1) = "C" ++ insereCaixa pcs (cc,lc) (c1+1,l1)
                                    | otherwise = showPeca p ++ insereCaixa pcs (cc,lc) (c1+1,l1)

{- |
seja o showPeca:

@
showPeca Bloco = "'X'"
showPeca Vazio = " "
showPeca Caixa = "'C'"
showPeca Porta = "'P'"
@
-}
{-| 
== Representação do Jogador
-}
showPeca :: Peca -- ^ Recebe uma peça 
         -> String -- ^ Devolve o respetivo representador
showPeca Bloco = "X"
showPeca Vazio = " "
showPeca Caixa = "C"
showPeca Porta = "P"

{- |
E o showJogador:

@ 
showJogador (Jogador (c,l) Este b) = ">"
showJogador (Jogador (c,l) Oeste b) = "<" 
@
-}

showJogador :: Jogador -- ^ Recebe o jogador 
            -> String -- ^ Devolve ">" ou "<" dependendo da direção para a qual está voltado 
showJogador (Jogador (c,l) Este b) = ">"
showJogador (Jogador (c,l) Oeste b) = "<" 