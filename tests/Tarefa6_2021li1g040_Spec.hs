module Tarefa6_2021li1g040_Spec where

import Test.HUnit
import Fixtures 
import LI12122
import Tarefa6_2021li1g040
import Tarefa2_2021li1g040 (desconstroiMapa)
import Data.Maybe (Maybe(Just))


testsT6 =
  test
    [ "1-Tarefa6 - Jogo simples sem caixas com limite de moves pequeno" ~: Just [AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda] ~=? resolveJogo 10 m2e1,
      "2-Tarefa6 - Jogo simples sem caixas com com limite de moves grande" ~: Just [AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda] ~=? resolveJogo 100 m2e1,
      "3-Tarefa6 - Verificação de existência de caixas no mapa" ~: False ~=? verifCaixaValida (desconstroiMapa m2r) (desconstroiMapa m2r),
      "4-Tarefa6 - Verificação de existência de caixas inúteis no mapa" ~: False ~=? verifCaixaValida (desconstroiMapa m2rv2) (desconstroiMapa m2rv2),
      "5-Tarefa6 - Verificação de existência de caixas inúteis no mapa" ~: False ~=? verifCaixaValida (desconstroiMapa m2rv3) (desconstroiMapa m2rv3),
      "6-Tarefa6 - Verificação de existência de caixas inúteis no mapa" ~: True ~=? verifCaixaValida (desconstroiMapa m2rv4) (desconstroiMapa m2rv4),
      "7-Tarefa6 - Verificação de existência de caixas inúteis no mapa" ~: True ~=? verifCaixaValida (desconstroiMapa m2rv4) (desconstroiMapa m2rv4),
      "8-Tarefa6 - Verificação do sistema de flags" ~: Just [AndarDireita,AndarDireita, AndarEsquerda,InterageCaixa,AndarEsquerda,
                                                             Trepar,AndarDireita,InterageCaixa,Trepar,Trepar,AndarDireita,AndarDireita] ~=? resolveJogo 15 m3e1
    ]
