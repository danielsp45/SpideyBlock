module Tarefa2_2021li1g040_Spec where

import Data.List (sort)
import Test.HUnit
import LI12122
import Tarefa2_2021li1g040
import Fixtures

testsT2 =
  test
    [ " Tarefa 2 - Teste Construir Mapa m1" ~: m1r ~=? constroiMapa m1
    , " Tarefa 2 - Teste Construir Mapa vazio" ~: [] ~=? constroiMapa []
    , " Tarefa 2 - Teste Desconstruir Mapa m1" ~: sort m1 ~=?  sort (desconstroiMapa m1r)
    , " Tarefa 2 - Teste Desconstruir Mapa vazio" ~: [] ~=? desconstroiMapa []
    , " Tarefa 2 - Teste desconstruir Mapa em escada" ~:[(Porta,(0,1)),(Bloco,(0,2)),(Bloco,(1,2)),(Bloco,(6,2)),(Bloco,(2,3)),(Bloco,(5,3)),(Bloco,(2,4)),(Bloco,(5,4)),(Bloco,(3,5)),(Bloco,(4,5))] ~=? desconstroiMapa [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Porta,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco],[Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Vazio],[Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Vazio],[Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio]]
    , " Tarefa 2 - Teste Identidade m1" ~: sort m1 ~=?  sort (desconstroiMapa (constroiMapa m1))
    , " Tarefa 2 - Teste Identidade m1r" ~: m1r ~=?  constroiMapa (desconstroiMapa m1r)
    , " Tarefa 2 - Teste Construir Sobrepor Peças" ~: constroiMapa [(Porta, (7, 4)),(Porta, (7, 4))] ~=? []
    , " Tarefa 2 - Teste Construir Mapa só com 1 peça" ~: constroiMapa [(Porta, (7, 4))] ~=? []
    , " Tarefa 2 - Teste Construir Mapa só com 1 peça" ~: constroiMapa [(Bloco,(0,2)),(Bloco,(0,3)),(Bloco,(0,4)),(Bloco,(0,5)),(Bloco,(0,6)),(Bloco,(0,7)),(Bloco,(0,8)),(Bloco,(0,9)),(Bloco,(0,10)),(Bloco,(1,0)),(Bloco,(1,1)),(Porta,(1,9)),(Bloco,(1,10)),(Bloco,(2,2)),(Bloco,(2,6)),(Bloco,(2,7)),(Bloco,(2,8)),(Bloco,(2,9)),(Bloco,(2,10)),(Bloco,(3,3)),(Bloco,(3,6)),(Bloco,(4,2)),(Bloco,(4,6)),(Bloco,(4,7)),(Bloco,(4,8)),(Bloco,(4,9)),(Bloco,(4,10)),(Bloco,(5,1)),(Caixa,(5,8)),(Bloco,(5,9)),(Bloco,(5,10)),(Bloco,(6,1)),(Caixa,(6,8)),(Bloco,(6,9)),(Bloco,(7,1)),(Bloco,(7,9)),(Bloco,(8,1)),(Bloco,(8,8)),(Bloco,(8,9)),(Bloco,(9,1)),(Bloco,(9,7)),(Bloco,(9,8)),(Bloco,(9,9)),(Bloco,(9,10)),(Bloco,(10,1)),(Bloco,(10,10)),(Bloco,(11,1)),(Bloco,(11,9)),(Bloco,(11,10)),(Bloco,(12,1)),(Bloco,(12,7)),(Bloco,(12,8)),(Bloco,(12,9)),(Bloco,(13,1)),(Bloco,(13,6)),(Bloco,(13,7)),(Bloco,(14,1)),(Caixa,(14,6)),(Bloco,(14,7)),(Bloco,(15,1)),(Bloco,(15,7)),(Bloco,(16,1)),(Caixa,(16,5)),(Bloco,(16,6)),(Bloco,(16,7)),(Bloco,(17,1)),(Caixa,(17,4)),(Caixa,(17,5)),(Bloco,(17,6)),(Bloco,(18,2)),(Bloco,(18,3)),(Bloco,(18,4)),(Bloco,(18,5))] ~=? 
  [[Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio],[Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],[Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Caixa,Vazio,Bloco,Bloco,Vazio],[Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio],[Bloco,Vazio,Bloco,Vazio,Bloco,Caixa,Caixa,Vazio,Bloco,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Bloco,Porta,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]]
    , " Tarefa 2 - Constroi Mapa só com uma linha de chaos no teto" ~: constroiMapa [(Bloco,(0,0)),(Bloco,(1,0)),(Bloco,(2,0)),(Porta,(2,1))] ~=? []
    , " Tarefa 2 - Desconstroi Mapa só com uma linha de chaos no teto" ~: desconstroiMapa [[Bloco, Bloco, Bloco],[Vazio,Vazio,Porta]] ~=? [(Bloco,(0,0)),(Bloco,(1,0)),(Bloco,(2,0)),(Porta,(2,1))] 
    , " Tarefa 2 - Constroi Mapa com 1 espaço vazio a baixo do chão" ~: constroiMapa [(Bloco,(0,0)),(Bloco,(1,1)),(Bloco,(2,0)),(Porta,(1,0)),(Vazio,(0,1)),(Vazio,(2,1))] ~=? []
    , " Tarefa 2 - Constroi Mapa com 1 espaço vazio a cima do chão" ~: constroiMapa [(Bloco,(0,0)),(Bloco,(1,1)),(Bloco,(2,1)),(Porta,(1,0)),(Vazio,(2,0))] ~=? [[Bloco,Porta,Vazio],[Vazio,Bloco,Bloco]]
    ]


    
