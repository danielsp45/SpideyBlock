{- |
Module      : Tarefa2_2021li1g040
Description :  Resolução de um puzzle
Copyright   : Daniel da Silva Pereira <a100545@alunos.uminho.pt>;
            : Rodrigo Viana Ramos Casal Novo <a100534@alunos.uminho.pt>;

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.

= BOT 

O objetivo final desta tarefa seria encontrar uma forma de automatizar a resolução 
de um mapa, recebendo apenas um número máximo de movimentos. Inicialmente foi tentada uma abordagem
de encontrar uma forma geral da resolução dos mapas, por exemplo, andar como jogador na direção da porta e caso não seja possível, 
voltar atrás e encontrar caixas e voltar a tentar resolver.
Obviamente, que dada a complexidade e diversidade que os mapas podem ter, esta não seria uma forma viável de fazer este bot, 
como muitas outras formas pensadas ao estilo desta. 
Haveriam certamente várias maneiras de o resolver mas, a forma utilizada, neste caso, explicada de uma forma sucinta, 
foi através do calculo dos caminhos possíveis dentro do limite de movimentos fornecidos pelo utilizador. 
Obviamente que esta forma apresenta um ponto fraco que é a eficiência do programa, tendo sido este o maior desâfio da tarefa, 
ou seja, tentar arranjar maneiras de reverter esta ineficiência.

Para resolver este bot, comecei por fazer um construtor de uma árvore com quatro ramos (__@BTree@__), 
um para cada movimento diponível, que receberia inicialmente os @Parametros@ iniciais do jogo e, partir daí, 
construir uma árvore, com várias funções auxiliares, dentro dos limites fornecidos. 
Para encurtar a árvore o máximo possível, foram inseridas uma série de medidas para terminar com ramos que tivessem movimentos inúteis, 
como por exemplo, andar contra um bloco.
A par disto, foi criado um sistema de flags, em que um movimento inútil causaria a uma variável nos @Parametros@ ser alterada para 1, 
e se dois movimentos seguidos fossem inuteis, o ramo seria cortado ali.

Inicialmente a função __@resTConstroi@__ retornava uma árvore completa, mas para facilitar a função __@limiter@__, 
a função passou a retornar apenas as folhas da árvore dentro de uma lista. A função limiter, recebe o número de movimentos totais, 
e em vez de correr a __@resTConstroi@__ de uma vez só, executa-a movimento a movimento para permitir que ela façaa verificação de que se há ou não 
um ramo da árvore que já tenha encontrado a solução do mapa. Desta forma, ao receber 100 movimentos para um jogo que seria 
facilmente resolvido em 10, o bot iria parar de encontrar uma solução depois desse ponto. 

Uma outra observação feita foi que, se um mapa não tiver caixas, ou as que tiver forem inutilizáveis, não valeria a pena fazer com que o bot 
calculasse resoluções possíveis onde integrasse o movimento /InterageCaixa/, resultando na função __@verifCaixaValida@__.

-}

module Tarefa6_2021li1g040 where
import LI12122
import Tarefa4_2021li1g040 
import Tarefa1_2021li1g040 ( validaCaixa )
import Tarefa2_2021li1g040 ( desconstroiMapa )

{-| A @RTree@ corresponde ao formato da árvore de resolução onde cada nodo filho vai corresponder a um movimento 
(/AndarEsquerda,InterageCaixa,Trepar,AndarDireita/)-}
data RTree a = Empty | Node a (RTree a) (RTree a) (RTree a) (RTree a)   --Rose tree
    deriving Show 

-- |  A @RTree@ é uma árvore em que recebe os diferentes @Parametros@
type ResTree = RTree Parametros --RTree em que recebe os diferentes parametros para a resolução do jogo

-- | Os @Parametros@ são informações, mutáveis ou não, que cada nodo deve levar consigo
type Parametros = (Movimento, NMoves, Jogo, InPorta, LMoves, Flag, Caixas) --Diferentes parametros que a RTree vai receber 

-- | A @Flag@ é do tipo Int e é utilizada para avisar o codigo da existência de movimentos inúteis
type Flag = Int 
-- | O @NMoves@ é um contador do tipo Int
type NMoves = Int 
-- | O @InPorta@ é um bool para avisar o código se o jogador já chegou à porta ou não 
type InPorta = Bool 
-- | O @LMoves@ corresponde a uma lista de movimentos
type LMoves = [Movimento]
-- | O @caixas@ é do tipo Bool e não é mutável durante o __@resTConstroi@__, servindo para alertar se o jogo tem caixas utilizáveis ou não
type Caixas = Bool

{- | A função __@resolveJogo@__ é a função principal desta tarefa, e vai receber o número de movimentos máximos, e o @Jogo@.
A partir daí, vai ativar a função __@limiter@__-}
resolveJogo :: Int -> Jogo -> Maybe LMoves
resolveJogo n jogo = limiter n [(Node (Nenhum, 0, jogo, False, [], 0, verifCaixaValida mapa' mapa') Empty Empty Empty Empty)]
                                                      
    where (Jogo mapa (Jogador (c,l) direcao caixa)) = jogo 
          mapa' = desconstroiMapa mapa 

{- | A função __@resTConstroi@__ é a função responsável por calcular a sequência de movimentos, deixando de fora os ramos que contenham movimentos inúteis 
a uma possível resolução.
-} 
resTConstroi :: Int -> ResTree -> [ResTree]
resTConstroi n Empty = []
resTConstroi n (Node (mov,nMoves, jogo, inPorta, lMoves, flag, existsCaixas) aEsquerda iCaixa trepar aDireita) 
    | nMoves > n = [(Node (mov,0, jogo, inPorta, lMoves, flag, existsCaixas) aEsquerda iCaixa trepar aDireita)]
    | otherwise =  (if (jogo == movAndarEsquerda || (flag == 1 && (mov == AndarDireita))) 
                      then [] 
                      else resTConstroi n (Node (AndarEsquerda, nMoves+1, movAndarEsquerda, isInPorta (c1,l1) mapa 0, lMoves ++ [AndarEsquerda], if mov == AndarDireita then 1 else 0, existsCaixas) aEsquerda iCaixa trepar aDireita)) ++ --AndarEsquerda
                   (if jogo == movInterageCaixa || mov == InterageCaixa || not existsCaixas 
                      then []
                      else resTConstroi n (Node (InterageCaixa, nMoves+1, movInterageCaixa, isInPorta (c4,l4) mapa 0, lMoves ++ [InterageCaixa],0, existsCaixas) aEsquerda iCaixa trepar aDireita)) ++       --InterageCaixa
                   (if jogo == movTrepar 
                      then [] 
                      else resTConstroi n (Node (Trepar, nMoves+1, movTrepar, isInPorta (c3,l3) mapa 0, lMoves ++ [Trepar], 0, existsCaixas) aEsquerda iCaixa trepar aDireita)) ++                            --Trepar
                   (if (jogo == movAndarDireita) || (flag == 1 && (mov == AndarEsquerda))
                      then [] 
                      else resTConstroi n (Node (AndarDireita, nMoves+1, movAndarDireita, isInPorta (c2,l2) mapa 0, lMoves ++ [AndarDireita],if mov == AndarEsquerda then 1 else 0, existsCaixas) aEsquerda iCaixa trepar aDireita))    --AndarDireita
    
    where (Jogo mapa (Jogador (c,l) direcao caixa)) = jogo 
          
          movAndarEsquerda = moveJogador jogo AndarEsquerda 
          (Jogo mapa1 (Jogador (c1,l1) direcao1 caixa1))= movAndarEsquerda
          
          movAndarDireita = moveJogador jogo AndarDireita 
          (Jogo mapa2 (Jogador (c2,l2) direcao2 caixa2))= movAndarDireita
          
          movTrepar = moveJogador jogo Trepar
          (Jogo mapa3 (Jogador (c3,l3) direcao3 caixa3))= movTrepar

          movInterageCaixa = moveJogador jogo InterageCaixa
          (Jogo mapa4 (Jogador (c4,l4) direcao4 caixa4))= movInterageCaixa

{- | A função __@isInPorta@__ faz a verificação de se o jogador já chegou a porta, retornando True caso isto se verifique.
Ele recebe as coordenadas do jogador, o mapa e o terceiro parametro que recebe é um acumulador. 
-}
isInPorta :: Coordenadas -> Mapa -> Int -> Bool 
isInPorta (c,l) [] _ = False 
isInPorta (c,l) (s:xs) acc | l == acc = isInPorta' c s 0 
                           | otherwise = isInPorta (c,l) xs (acc+1)
-- | Função auxiliar de __@isInPorta@__
isInPorta' :: Int -> [Peca] -> Int -> Bool 
isInPorta' c [] _ = False 
isInPorta' c (s:xs) n | c == n && s == Porta = True 
                      | otherwise = isInPorta' c xs (n+1)

{- | Esta funcao retorna True se existirem caixas no mapa e se essas caixas forerm utilizaveis.
A função recebe o mapa duas vezes, visto que um deles serverá apenas para passar às funções auxiliares __@verifCaixaCima@__ e __@verifCaixaLados@__
-}
verifCaixaValida :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> Bool 
verifCaixaValida mapa [] = False
verifCaixaValida mapa ((peca,(c,l)):xs) | peca == Caixa = (verifCaixaCima mapa (c,l) && verifCaixaLados mapa (c,l) 0) || verifCaixaValida mapa xs
                                        | otherwise = verifCaixaValida mapa xs
 
-- | Verifica se existem blocos por cima das caixas 
verifCaixaCima :: [(Peca, Coordenadas)] -> Coordenadas -> Bool 
verifCaixaCima [] (c,l) = True
verifCaixaCima ((p1,(c1,l1)):xs) (c,l) | (c1 == c) && (l1 == l-1) && (p1 /= Bloco) = True 
                                          | (c1 == c) && (l1 == l-1) && (p1 == Bloco) = False 
                                          | otherwise = verifCaixaCima xs (c,l)

-- | Verifica se existem dois blocos ao lado da caixa, um em cada lado
verifCaixaLados :: [(Peca, Coordenadas)] -> Coordenadas -> Int -> Bool
verifCaixaLados [] (c,l) flag = True 
verifCaixaLados ((p1,(c1,l1)):xs) (c,l) flag | ((c1 == c+1) || (c1 == c-1)) && (l1 == l) && (p1 /= Bloco) = True 
                                                | ((c1 == c+1) || (c1 == c-1)) && (l1 == l) && (p1 == Bloco) = if flag == 1 then False else verifCaixaLados xs (c,l) 1
                                                | otherwise = verifCaixaLados xs (c,l) flag

----------------
{- | A função __@limiter@__ é chamada em __@resolveJogo@__ e recebe o limite de movimentos imposto nela e o primeiro nodo com
o dados iniciais do jogo. 
Esta função em vez de inserir o limite de movimentos todos de uma vez na função __@resolveJogo@__, permite apenas que a função 
calcule um movimento, de forma a poder analisar a lista fornecida e ver se encontrou a resolução do jogo ou não.
-}
limiter :: Int -> [ResTree] -> Maybe LMoves
limiter n [] = Nothing
limiter n nodes
    | n == 0 = Nothing 
    | checker nodes = Just $ movesExtractor nodes 
    | otherwise = limiter (n-1) (recursiveResT nodes)

-- | Esta função verifica se a resolução do jogo está presente na lista de nodos
checker :: [ResTree] -> Bool
checker [] = False 
checker (s:xs) = checker' s || checker xs   
-- | Função auxiliar de __@checker@__
checker' :: ResTree -> Bool 
checker' Empty = False 
checker' (Node (mov,nMoves, jogo, inPorta, lMoves, flag, caixa) aEsquerda iCaixa trepar aDireita)  
    | inPorta = True
    | otherwise = False 

{- | Quando uma resolução é encontrada, esta função é chamada, para poder retirar a lista de movimentos do nodo que retornou /True/
para o __@inPorta@__ -}
movesExtractor :: [ResTree] -> LMoves
movesExtractor (Empty:xs) = movesExtractor xs
movesExtractor [] = []
movesExtractor ((Node (mov,nMoves, jogo, inPorta, lMoves, flag, caixa) a b c d):xs)
    | inPorta = lMoves 
    | otherwise = movesExtractor xs

-- | A __@recursiveResT@__ aplica o __@resTConstroi@__ à lista de nodos permitindo apenas um movimento máximo, retornando uma lista de nodos.
recursiveResT :: [ResTree] -> [ResTree]
recursiveResT [] = []
recursiveResT (s:xs) = (resTConstroi 1 s) ++ recursiveResT xs



j1 = Jogo mapa1 (Jogador (6,4) Oeste False)
j2 = Jogo mapa2 (Jogador (8,5) Oeste False)
j3 = Jogo mapa3 (Jogador (17, 7) Oeste False)
j4 = Jogo mapa4 (Jogador (1,7) Este True)
j5 = Jogo mapa5 (Jogador (19,7) Oeste False)


mapa1 :: Mapa
mapa1 = [
      [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Bloco]
    , [Bloco, Bloco, Bloco, Bloco, Vazio, Bloco, Bloco, Bloco]
    , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

mapa2 :: Mapa
mapa2 = [
      [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Bloco]
    , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

mapa3 :: Mapa
mapa3 = [ 
      [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Porta, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Caixa, Vazio, Bloco, Vazio, Vazio, Caixa, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]


mapa4 :: Mapa
mapa4 = [ 
      [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Porta, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Caixa, Vazio, Bloco, Vazio, Caixa, Bloco, Bloco, Vazio, Bloco, Bloco]
    , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]



mapa5 :: Mapa
mapa5 = [ 
      [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Porta, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Bloco]
    , [Bloco, Caixa, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Bloco, Bloco]
    , [Bloco, Caixa, Caixa, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Bloco, Bloco, Bloco, Vazio, Caixa, Bloco, Vazio, Bloco, Bloco, Bloco, Vazio, Bloco, Bloco]
    , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]
