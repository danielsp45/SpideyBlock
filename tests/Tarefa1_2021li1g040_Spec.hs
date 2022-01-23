module Tarefa1_2021li1g040_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g040
import Fixtures

-- Tarefa 1
testsT1 =
  test
    [ "Tarefa 1 - Teste Valida Mapa m1r" ~: validaPotencialMapa m1 ~=? True
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: validaPotencialMapa [] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com 2 portas" ~: validaPorta [(Porta, (0,0)), (Porta, (1,0))] ~=?  False
    , "Tarefa 1 - Teste Valida Mapa com caixa a flotuar" ~: validaPotencialMapa [(Caixa,(0,0)),(Bloco, (0,3)),(Bloco,(1,2)),(Porta,(0,1))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com caixa no chão" ~: validaPotencialMapa [(Caixa,(0,0)),(Bloco, (0,1)),(Bloco,(1,2)),(Porta,(1,0))] ~=? True
    , "Tarefa 1 - Teste Valida Mapa com caixa em cima de outra caixa" ~:validaPotencialMapa [(Caixa,(0,0)),(Caixa,(0,1)),(Bloco, (0,2)),(Bloco,(1,2)),(Porta,(1,0))] ~=? True
    , "Tarefa 1 - Teste Valida Mapa com espaço cheio mas 1 ocupado com vazio" ~:validaPotencialMapa [(Bloco,(0,0)),(Vazio,(1,0)),(Porta,(2,0)),(Bloco,(0,1)),(Bloco,(1,1)),(Bloco,(2,1))] ~=? True
    , "Tarefa 1 - Teste Valida Mapa com 1 unico espaço vazio omitido" ~:validaPotencialMapa [(Bloco,(0,0)),(Porta,(2,0)),(Bloco,(0,1)),(Bloco,(1,1)),(Bloco,(2,1))] ~=? True
    , "Tarefa 1 - Teste Valida Mapa com duas portas" ~: validaPotencialMapa [(Bloco,(0,3)),(Bloco, (1,3)),(Bloco, (1,4)),(Bloco,(1,5)),(Bloco,(2,5)),(Bloco,(3,5)),(Bloco,(4,5)),(Bloco,(4,4)),(Bloco,(4,3)),(Bloco,(5,3)),(Bloco, (0,0)),(Bloco, (3,0)),(Bloco,(2,3)),(Caixa,(5,2)), (Porta,(0,2)),(Porta,(5,5))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com 2 peças na mesma posição" ~: validaPosicao [(Bloco,(0,3)), (Porta,(0,3))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com 2 portas 1 em cima do chão e outra em baixo" ~: validaPotencialMapa [(Bloco,(0,3)),(Bloco, (1,3)),(Bloco, (1,4)),(Bloco,(1,5)),(Bloco,(2,5)),(Bloco,(3,5)),(Bloco,(4,5)),(Bloco,(4,4)),(Bloco,(4,3)),(Bloco,(5,3)),(Bloco, (0,0)),(Bloco, (3,0)),(Bloco,(2,3)),(Caixa,(5,2)), (Porta,(0,2)),(Porta,(5,5))] ~=? False
    , "Tarefa 1 - Teste Valida Chão sem 1 bloco na primeira linha" ~: validaChao [(Bloco,(1,1))] ~=? False
    , "Tarefa 1 - Teste Valida Chão com 1 bloco na primeira linha" ~: validaChao [(Bloco,(0,4))] ~=? True
    , "Tarefa 1 - Teste Valida Chão com um salto na horizontal" ~: validaChao [(Bloco,(0,0)),(Bloco,(2,0))] ~=? False
    , "Tarefa 1 - Teste Valida Chão com um salto na vertical" ~: validaChao [(Bloco,(0,0)),(Bloco,(1,0)),(Bloco,(1,1)),(Bloco,(1,3)),(Bloco,(2,3))] ~=? False
    , "Tarefa 1 - Teste Valida Chão com chão na diagonal" ~: validaChao [(Bloco,(0,0)),(Bloco,(1,1)),(Bloco,(2,2)),(Bloco,(3,1)),(Bloco,(4,0))] ~=? True
    , "Tarefa 1 - Teste Valida Chão com um bloco possivel á frente do caminho"~: validaChao [(Bloco,(0,3)),(Bloco, (1,3)),(Bloco, (2,3)),(Bloco, (1,4)),(Bloco,(1,5)),(Bloco,(2,5)),(Bloco,(3,5)),(Bloco,(4,4)),(Bloco,(4,3)),(Bloco,(4,5)),(Bloco,(5,3)),(Bloco, (0,0)),(Bloco, (3,0))] ~=? True
    , "Tarefa 1 - Teste Valida Mapa com chão em escada" ~: validaPotencialMapa [(Porta,(0,1)),(Bloco,(0,2)),(Bloco,(1,2)),(Bloco,(6,2)),(Bloco,(2,3)),(Bloco,(5,3)),(Bloco,(2,4)),(Bloco,(5,4)),(Bloco,(3,5)),(Bloco,(4,5))] ~=? True
    , "Tarefa 1 - Teste Valida Mapa com o primeiro mapa do segundo inunciado do chão" ~: validaPotencialMapa [(Bloco,(0,2)),(Bloco,(0,3)),(Bloco,(0,4)),(Bloco,(0,5)),(Bloco,(0,6)),(Bloco,(0,7)),(Bloco,(0,8)),(Bloco,(0,9)),(Bloco,(0,10)),(Bloco,(1,0)),(Bloco,(1,1)),(Porta,(1,9)),(Bloco,(1,10)),(Bloco,(2,2)),(Bloco,(2,6)),(Bloco,(2,7)),(Bloco,(2,8)),(Bloco,(2,9)),(Bloco,(2,10)),(Bloco,(3,3)),(Bloco,(3,6)),(Bloco,(4,2)),(Bloco,(4,6)),(Bloco,(4,7)),(Bloco,(4,8)),(Bloco,(4,9)),(Bloco,(4,10)),(Bloco,(5,1)),(Caixa,(5,8)),(Bloco,(5,9)),(Bloco,(5,10)),(Bloco,(6,1)),(Caixa,(6,8)),(Bloco,(6,9)),(Bloco,(7,1)),(Bloco,(7,9)),(Bloco,(8,1)),(Bloco,(8,8)),(Bloco,(8,9)),(Bloco,(9,1)),(Bloco,(9,7)),(Bloco,(9,8)),(Bloco,(9,9)),(Bloco,(9,10)),(Bloco,(10,1)),(Bloco,(10,10)),(Bloco,(11,1)),(Bloco,(11,9)),(Bloco,(11,10)),(Bloco,(12,1)),(Bloco,(12,7)),(Bloco,(12,8)),(Bloco,(12,9)),(Bloco,(13,1)),(Bloco,(13,6)),(Bloco,(13,7)),(Bloco,(14,1)),(Caixa,(14,6)),(Bloco,(14,7)),(Bloco,(15,1)),(Bloco,(15,7)),(Bloco,(16,1)),(Caixa,(16,5)),(Bloco,(16,6)),(Bloco,(16,7)),(Bloco,(17,1)),(Caixa,(17,4)),(Caixa,(17,5)),(Bloco,(17,6)),(Bloco,(18,2)),(Bloco,(18,3)),(Bloco,(18,4)),(Bloco,(18,5))] ~=? True
    , "Tarefa 1 - Teste se Vazios por baixo do chão são ignorados" ~: validaPotencialMapa [(Vazio,(0,2)),(Vazio,(1,2)),(Bloco,(1,1)),(Bloco,(0,1)),(Porta,(0,0)),(Bloco,(1,0))] ~=? False 
    ]
