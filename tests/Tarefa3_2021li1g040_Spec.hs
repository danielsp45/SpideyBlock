module Tarefa3_2021li1g040_Spec where

import Test.HUnit
import Tarefa3_2021li1g040
import Fixtures
import LI12122

testsT3 =
  test
    [ "Tarefa 3 - Teste Imprime Jogo m1e1" ~: "      <\n      X\n      X\nP   C X\nXXXXXXX" ~=?  show m1e1
    , "Tarefa 3 - Teste Imprime Jogo m1e2" ~: "       \n      X\n      X\nP < C X\nXXXXXXX" ~=?  show m1e2
    , "Tarefa 3 - Teste Jogo imprimido com jogador com caixa" ~: "       \n  C   X\n  <   X\nP   C X\nXXXXXXX" ~=? show( Jogo [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco] , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco] , [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco] , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]] (Jogador (2,2) Oeste True))
    , "Tarefa 3 - Teste Jogo imprimido com jogador com caixaem maxima altura" ~: "       \n      X\n     CX\nP   C<X\nXXXXXXX" ~=? show (Jogo [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco] , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco] , [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco] , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]] (Jogador (5,3) Oeste True))
    , "Tarefa 3 - Teste se imprime mapas com chão não plano " ~: "       \nP    C \nXX   <X\n  X  X \n  X  X \n   XX  " ~=? show ( Jogo [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Porta,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco],[Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Vazio],[Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Vazio],[Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio]](Jogador (5,2) Oeste True))
    , "Tarefa 3 - Teste Imprimir Mapa com todos os possiveis casos" ~: " X                 \n X   XXXXXXXXXXXXX \nX X X             X\nX  X              X\nX                CX\nX               CCX\nX XXX        XC<XX \nX X X    X  XXXXX  \nX X XCC XX  X      \nXPX XXXXXX XX      \nXXX XX   XXX       "
      ~=? show (Jogo [[Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio],[Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],[Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Caixa,Vazio,Bloco,Bloco,Vazio],[Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio],[Bloco,Vazio,Bloco,Vazio,Bloco,Caixa,Caixa,Vazio,Bloco,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Bloco,Porta,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]] (Jogador (15,6) Oeste False))
    , "Tarefa 3 - Teste com jogador fora do mapa" ~: "      X\n      X\n     CX\nP    XX\nXXXXXXX" ~=? show(Jogo m1rv4 (Jogador (7,7) Este False))
    , "Tarefa 3 - Teste com Jogodor fora do maoa" ~: "      X\n      X\n     CX\nP    XX\nXXXXXXX" ~=? show(Jogo m1rv4 (Jogador (-1,-1) Este False))
    ]