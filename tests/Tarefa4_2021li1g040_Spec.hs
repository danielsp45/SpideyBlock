module Tarefa4_2021li1g040_Spec where

import Test.HUnit
import Fixtures
import Tarefa4_2021li1g040
import LI12122 
import Tarefa1_2021li1g040 
testsT4 =
  test
    [ "1-Tarefa 4 - Teste Move m1e1 Oeste" ~: Jogo m1r (Jogador (5, 3) Oeste False) ~=? moveJogador m1e01 AndarEsquerda
    , "2-Tarefa 4 - Teste Move m1e1 Este" ~: Jogo m1r (Jogador (6, 0) Este False) ~=? moveJogador m1e1 AndarDireita
    , "3-Tarefa 4 - Teste Move m1e1 Trepar" ~: Jogo m1r (Jogador (4, 2) Oeste False) ~=? moveJogador m1e01 Trepar
    , "4-Tarefa 4 - Teste Move m1e1 InterageCaixa" ~: Jogo 
    [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]] (Jogador (5, 3) Oeste True) ~=?  moveJogador m1e01 InterageCaixa
    , "5-Tarefa 4 - Teste movimentos m1e1" ~: m1e2 ~=?  correrMovimentos m1e01 [AndarEsquerda, Trepar, AndarEsquerda, AndarEsquerda]
    , "6-Tarefa 4 - Teste movimentos m1e2 Caixa1" ~: Jogo 
        [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
        (Jogador (3, 3) Este True) ~=?  correrMovimentos m1e2 [AndarDireita, InterageCaixa]
    , "7-Tarefa 4 - Teste movimentos m1e2 Caixa2" ~: 
        Jogo
        [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Porta, Caixa, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
        (Jogador (2, 3) Oeste False) ~=?  correrMovimentos m1e2 [AndarDireita, InterageCaixa, AndarEsquerda, InterageCaixa]
    , "8-Tarefa 4 - Teste posicionaBoneco" ~: (5,2) ~=?  posicionaBonecoCaixa [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]] (5,0)
    , "9-Tarefa 4 - CASO 1" ~: Jogo 
        [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
        [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
        [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
        (Jogador (5,3) Oeste False) ~=? correrMovimentos (Jogo m1r (Jogador (5,0) Oeste False)) [AndarEsquerda] 
    ,"10-Tarefa 4 - Teste de movimento com obstrucao" ~: Jogo 
        [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
        [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
        [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
        (Jogador (5,3) Este False) ~=? correrMovimentos (Jogo m1r (Jogador (5,3) Este False)) [AndarDireita] 
    ,"11-Tarefa 4 - Teste de InterageCaixa x2" ~: Jogo 
        [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
        [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
        [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
        (Jogador (3,3) Este False) ~=? correrMovimentos (Jogo m1r (Jogador (3,3) Este False)) [InterageCaixa,InterageCaixa]
    ,"12-Tarefa 4 - Teste de InterageBloco com obstrucao " ~: Jogo 
        [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
        [Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco],
        [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
        [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
        (Jogador (5,3) Oeste False) ~=? correrMovimentos (Jogo m1rv2 (Jogador (5,3) Oeste False)) [InterageCaixa] 
    ,"13-Tarefa 4 - Teste de InterageCaixa de posicao alta" ~: Jogo 
        [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
        [Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Bloco],
        [Porta, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco],
        [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
        (Jogador (4,3) Este False) ~=? correrMovimentos (Jogo m1rv4 (Jogador (4,3) Este False)) [InterageCaixa]
    ,"14-Tarefa 4 - Teste de InterageCaixa basico" ~: Jogo 
        [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
        [Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco],
        [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
        [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
        (Jogador (3,3) Este False) ~=? correrMovimentos (Jogo m1rv2 (Jogador (3,3) Este False)) [InterageCaixa]
    ,"15-Tarefa 4 - Teste de largar caixa de posicao alta" ~: Jogo 
        [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
        [Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Bloco],
        [Porta, Vazio, Vazio, Vazio, Caixa, Bloco, Bloco],
        [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
        (Jogador (5,2) Oeste False) ~=? correrMovimentos (Jogo m1rv5 (Jogador (5,2) Oeste True)) [InterageCaixa]
    ,"16-Tarefa 4 - Teste de InterageCaixa " ~: Jogo 
        [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
        [Porta, Vazio, Vazio, Vazio, Caixa, Caixa, Bloco],
        [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
        (Jogador (4,2) Este False) ~=? correrMovimentos (Jogo m1r (Jogador (4,2) Este True)) [InterageCaixa]
    ,"17-Tarefa4 PosicionaCaixa" ~:
        [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
        [Porta, Vazio, Vazio, Caixa, Caixa, Caixa, Bloco],
        [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
        ~=? posicionaCaixa Este (2,1) 
        [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
        [Porta, Vazio, Vazio, Vazio, Caixa, Caixa, Bloco],
        [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
    ,"18-Tarefa 4 Obstrucao com caixa" ~: Jogo m1rv6 (Jogador (2,4) Este True) ~=? correrMovimentos (Jogo m1rv6 (Jogador (2,4) Este True)) [AndarDireita]

    ]

 