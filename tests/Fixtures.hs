module Fixtures where
import Tarefa4_2021li1g040
import LI12122

m1 :: [(Peca, Coordenadas)]
m1 =
  [ (Porta, (0, 3)),
    (Bloco, (0, 4)),
    (Bloco, (1, 4)),
    (Bloco, (2, 4)),
    (Bloco, (3, 4)),
    (Bloco, (4, 4)),
    (Caixa, (4, 3)),
    (Bloco, (5, 4)),
    (Bloco, (6, 4)), 
    (Bloco, (6, 3)),
    (Bloco, (6, 2)),
    (Bloco, (6, 1))
  ]

m1r :: Mapa
m1r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]
m1e1 :: Jogo
m1e1 = Jogo m1r (Jogador (6, 0) Oeste False)

m1e01 :: Jogo
m1e01 = Jogo m1r (Jogador (5, 0) Oeste False)

m1e2 :: Jogo
m1e2 = Jogo m1r (Jogador (2, 3) Oeste False)

--Versao 2 do m1r para testes
m1rv2 :: Mapa 
m1rv2 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]
m1rv3 :: Mapa 
m1rv3 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco],
    [Porta, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]
m1rv4 :: Mapa 
m1rv4 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Bloco],
    [Porta, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]
m1rv5 :: Mapa 
m1rv5 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Bloco],
    [Porta, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]
m1rv6 :: Mapa 
m1rv6 =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Bloco, Vazio, Caixa, Bloco],
    [Porta, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]


m2r :: Mapa
m2r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Porta, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]
m2e1 :: Jogo
m2e1 = Jogo m2r (Jogador (4, 3) Oeste False)

m2rv2 = 
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio],
    [Porta, Vazio, Caixa, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m2rv3 = 
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Porta, Bloco, Caixa, Bloco, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m2rv4 = 
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Porta, Bloco, Caixa, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m3r = 
  [ [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Caixa, Vazio, Bloco, Vazio, Porta, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m3e1 = Jogo m3r (Jogador (1, 2) Oeste False)

m4r :: Mapa
m4r = [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Porta, Vazio, Bloco, Vazio, Vazio, Vazio],
    [Bloco, Vazio, Caixa, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
    ] 
m4e1 = Jogo m4r (Jogador (3, 2) Oeste False)