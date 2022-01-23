{- |
Module      : Tarefa2_2021li1g040
Description : Aplicação gráfica completa
Copyright   : Daniel da Silva Pereira <a100545@alunos.uminho.pt>;
            : Rodrigo Viana Ramos Casal Novo <a100534@alunos.uminho.pt>;

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22 juntamente com novos tipos para auxilio nesta tarefa.
-}

module Main (
    -- * Tipos de Dados acrescentados
    -- ** Tipos
  World , OpcaoAtual,
    -- ** Datas
  Estado(..),Imagens(..),
    -- * Função Main
  main,
    -- * Funções Auxiliares
    -- ** Estado Inicial
  inicialState,
    -- ** Imagem do Estado Centrada
  printState,
    -- ** Alteração Com Eventos
  reageTecla,
    -- *** Imagens Dos Jogadores
  pictureJogador,pictureJogador',
    -- *** Desenho Do Mapa
  mapaToPicture,pecaToPicture, novoMapa, novoMapa'
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import LI12122
import Tarefa4_2021li1g040
import Tarefa2_2021li1g040 (desconstroiMapa)
import Graphics.Gloss.Interface.IO.Game (playIO)
import System.Exit (exitSuccess)
import Data.Maybe (fromJust)
import Mapas
-- | Tuplo com o 'Estado' atual e as 'Imagens' que precisaremos ao longo do programa.
type World = (Estado,Imagens)


-- | Par com o primeiro ou segundo menu e opção selecionada que diferencia cada modo.
type OpcaoAtual = (Int,Int)
-- | Estado atual ou possiveis
data Estado =
    Menu
        OpcaoAtual -- ^ Opção no menu selecionada
        String -- ^ Modo de jogo selecionado
        Int -- ^ Nível selecionado
        [Jogo] -- ^ Jogo gravado
    |Pause
       Int -- ^ Opção 1(cima) ou opção 2(baixo)
       Estado -- ^ Estado em que parou o Jogo
    |Playable
        ([Jogo],[Jogo]) -- ^ Par de Jogos, sendo o primeiro o atual(singleplayer) e o segundo a última gravação
        Picture -- ^ Imagem do 'mapa' atual
        Picture -- ^ Imagem do 'Jogador'
        Coordenadas -- ^ Coordenadas da Porta 
    |Multiplayer
        ([Jogo'],[Jogo]) -- ^ Par de Jogos, sendo o primeiro o atual(multiplayer) e o segundo a última gravação
        Picture -- ^ Imagem do 'mapa' atual
        Picture -- ^ Imagem do primeiro 'Jogador'
        Picture -- ^ Imagem do segundo 'Jogador'
        Coordenadas -- ^ Coordenadas da Porta 
        [Coordenadas] -- ^ Lista com todas as coordenadas das caixas no momento atual


-- | lista de imagens 

data Imagens =
    Imagens
   { pecas :: [(Peca,Picture)] -- ^ par 'peça' e imagem correspondente
   , jogador :: [(Direcao,Picture)] -- ^ par 'direção' e imagem correspondente do jogador 1
   , jogador' :: [(String,Picture)] -- ^ par 'direção' ou 'peça' auxiliares ao multiplayer (segundo jogador e esmagar com caixa)
   , backgraunds :: [(String,Picture)] -- ^ par local e respetiva imagem para backgrounds
   , botoes :: [(Int,Picture)] -- ^ par nivel/extras e respetiva imagem do botão para o menu
   }


-- |Estado Inicial
inicialState :: Estado -- ^ Começa no menu inicial com a primeira opção selecionada
inicialState = Menu (1,1) "Normal" 1 levelMaps
  where levelMaps = allGameMaps 1

coordPorta :: Mapa -> Coordenadas
coordPorta m = takecoord $ head (filter porta (desconstroiMapa m))
    where porta (p,_) = p==Porta
          takecoord (_,c) = c
-- | Imagem do jogador
pictureJogador :: Imagens -- ^ Recebe as 'Imagens' que vai utilizar
               -> Jogador -- ^ Recebe também o jogador que vai alterar
               -> Picture -- ^ Devolve a imagem escalada e na posição correspondente
pictureJogador img (Jogador (c,l) dir withBox) |dir == Oeste && withBox = pictures[Translate (fromIntegral (50*c)) (fromIntegral (50-50*l)) (scale 0.089 0.09 $ fromJust $ lookup Caixa $ pecas img),translate (fromIntegral (50*c)) (fromIntegral (-50*l)) $ scale 0.2 0.2 $ fromJust $ lookup "bracosAoArO" $ jogador' img]
                                               |withBox = pictures[Translate (fromIntegral (50*c)) (fromIntegral (50-50*l)) (scale 0.089 0.09 $ fromJust $ lookup Caixa $ pecas img), translate (fromIntegral (50*c)) (fromIntegral (-50*l)) $ scale 0.2 0.2 $fromJust $ lookup "bracosAoArE" $ jogador' img]
                                               |otherwise =  translate (fromIntegral (50*c)) (fromIntegral (-50*l)+1) $ scale 0.2 0.2 $fromJust $ lookup dir $ jogador img
{- |
O picture jogador é utilizado quando acontece alguma alteração por parte de algum dos jogadores,
esta alteração pode ser quer na direção, quer no booleano (carrega ou não caixa) ou, obviamente, nas coordenadas.

Caso não haja nenhuma destas situações o movimento é considerado inútil e é ignorado antes mesmo de esta função ser chamada.

O 'pictureJogador' refere-se ao jogador do singleplayer e ao jogador1 do multiplayer e o 'pictureJogador'' refere-se ao jogador2 do multiplayer 

@
pictureJogador img (Jogador (c,l) dir withBox) |dir == Oeste && withBox = pictures[Translate (fromIntegral (50*c)) (fromIntegral (50-50*l)) (scale 0.089 0.09 $ fromJust $ lookup Caixa $ pecas img),translate (fromIntegral (50*c)) (fromIntegral (-50*l)) $ scale 0.2 0.2 $ fromJust $ lookup "bracosAoArO" $ jogador' img]
                                               |withBox = pictures[Translate (fromIntegral (50*c)) (fromIntegral (50-50*l)) (scale 0.089 0.09 $ fromJust $ lookup Caixa $ pecas img), translate (fromIntegral (50*c)) (fromIntegral (-50*l)) $ scale 0.2 0.2 $fromJust $ lookup "bracosAoArE" $ jogador' img]
                                               |otherwise =  translate (fromIntegral (50*c)) (fromIntegral (-50*l)+1) $ scale 0.2 0.2 $fromJust $ lookup dir $ jogador img

pictureJogador' img (Jogador (c,l) dir withBox)|dir == Oeste && withBox = pictures[Translate (fromIntegral (50*c)) (fromIntegral (50-50*l)) (scale 0.089 0.09 $ fromJust $ lookup Caixa $ pecas img),translate (fromIntegral (50*c)) (fromIntegral (-50*l-1)) $ scale 0.2 0.2 $ fromJust $ lookup "Oeste" $ jogador' img]
                                               |dir == Oeste =  translate (fromIntegral (50*c)) (fromIntegral (-50*l-1)) $ scale 0.2 0.2 $ fromJust $ lookup ""Oeste"" $ jogador' img
                                               |dir == Este && withBox = pictures[Translate (fromIntegral (50*c)) (fromIntegral (50-50*l)) (scale 0.089 0.09 $ fromJust $ lookup Caixa $ pecas img), translate (fromIntegral (50*c)) (fromIntegral (-50*l-1)) $ scale 0.2 0.2 $fromJust $ lookup "Este" $ jogador' img]
                                               |otherwise =  translate (fromIntegral (50*c)) (fromIntegral (-50*l-1)) $ scale 0.2 0.2 $fromJust $ lookup ""Este"" $ jogador' img

@
-}
pictureJogador' :: Imagens -- ^ Recebe as 'Imagens' que vai utilizar
                -> Jogador -- ^ Recebe também o jogador que vai alterar
                -> Picture -- ^ Devolve a imagem escalada e na posição correspondente
pictureJogador' img (Jogador (c,l) dir withBox)|dir == Oeste && withBox = pictures[Translate (fromIntegral (50*c)) (fromIntegral (50-50*l)) (scale 0.089 0.09 $ fromJust $ lookup Caixa $ pecas img),translate (fromIntegral (50*c)) (fromIntegral (-50*l-1)) $ scale 0.2 0.2 $ fromJust $ lookup "Oeste" $ jogador' img]
                                               |dir == Oeste =  translate (fromIntegral (50*c)) (fromIntegral (-50*l-1)) $ scale 0.2 0.2 $ fromJust $ lookup "Oeste" $ jogador' img
                                               |dir == Este && withBox = pictures[Translate (fromIntegral (50*c)) (fromIntegral (50-50*l)) (scale 0.089 0.09 $ fromJust $ lookup Caixa $ pecas img), translate (fromIntegral (50*c)) (fromIntegral (-50*l-1)) $ scale 0.2 0.2 $fromJust $ lookup "Este" $ jogador' img]
                                               |otherwise =  translate (fromIntegral (50*c)) (fromIntegral (-50*l-1)) $ scale 0.2 0.2 $ fromJust $ lookup "Este" $ jogador' img


-- | Esta função percorre o mapa e a cada peça transforma numa picture e desloca-a para o sitio onde se enquadra através da função 'pecaToPicture'

mapaToPicture :: Imagens -- ^ Recebe as 'Imagens' que vai utilizar
              -> Mapa  -- ^ Recebe o mapa que vai transformar
              -> Coordenadas -- ^ Recebe as coordenadas da origem (0,0)
              -> [Picture] -- ^ Devolve o mapa em imagens com canto superior esquerdo na origem
mapaToPicture _ [] _ = []
mapaToPicture img ([]:resto) (_,l) = mapaToPicture img resto (0,l-50)
mapaToPicture img ((h:t):tls) (c,l) = pecaToPicture img (h,(c,l)) : mapaToPicture img (t:tls) (c+50,l)

{- |
Sem a importação de imagens teria de ser usado formas geométricas já predefinidas no Gloss em baixo está um exemplo de como o fizemos

@
data Figura
    = Quadrado Int Color (Int,Int) --bloco e caixa
    | Rectangulo Int Int Color (Int,Int) -- porta

figurasToPicture :: [Figuras] -> Picture
figurasToPicture l = Pictures $ map (figuraToPicuture) l 
figuraToPicture :: Figura -> Picture
figuraToPicture (Quadrado l c (coluna,linha)) = Translate (fromIntegral coluna) (fromIntegral linha) $ Color c $ rectangleSolid (fromIntegral l)  (fromIntegral l)
figuraToPicture (Rectangulo b l c (coluna,linha)) = Translate (fromIntegral coluna) (fromIntegral linha) $ Color c $ rectangleSolid (fromIntegral b)  (fromIntegral l)

mapaToPicture :: (Mapa,Coordenadas) -> [Figura]
mapaToPicture ([],_)= []
mapaToPicture ([]:resto,(_,l)) = mapaToPicture (resto,(0,l-50))
mapaToPicture ((x:resto):resto',(c,l)) | x == Bloco = Quadrado 50 black (c,l) : tail
                                       | x == Porta = Rectangulo 30 50 blue (c,l) : tail
                                       | x == Vazio = tail
                                       | x == Caixa = Quadrado 50 red (c,l) : tail
                                       | otherwise = error "input não é um mapa"
                    where tail = mapaToPicture (resto:resto',(c+50,l))
@
-}
pecaToPicture :: Imagens -- ^ Recebe as 'Imagens' que vai utilizar
              -> (Peca,Coordenadas) -- ^ Recebe uma peça e o local onde a colocar
              -> Picture -- ^ Devolve a imagem da peça na posição desejada
pecaToPicture img (x,(c,l)) | x == Bloco = Translate (fromIntegral c) (fromIntegral l) (scale 0.129 0.17 $ fromJust $ lookup Bloco $ pecas img)
                            | x == Porta = Translate (fromIntegral c+5) (fromIntegral l) (scale 0.09 0.095 $ fromJust $ lookup Porta $ pecas img)
                            | x == Caixa = Translate (fromIntegral c) (fromIntegral l) (scale 0.089 0.09 $ fromJust $ lookup Caixa $ pecas img)
                            | otherwise = Blank

{- | A função printState verifica o estado e, dependendo do mesmo desenha no ecrã o que acontece, isto é:

      - No caso de este ser um 'Menu' -> Devolve o background adequado, o botão selecionado aumentado, bem como o resto dos botões em tamanho normal;  
      - No caso de este ser um 'Playable' -> Devolve o background adequado e a junção da imagem do mapa com o jogador (escolhidos no 'reageTecla');  
      - No caso de este ser um 'Multiplayer' -> Devolve o background adequado e a junção da imagem do mapa com a imagem personalizada dos jogadores (escolhidos no 'reageTecla');
      - Por último pode ser um menu 'Pause' -> Devolve o background adequado, apresentação das imagens dos controls, a apresentação dos botões (do mesmo modo que o menu).

-}
printState :: World -- ^ Recebe o 'Estado' atual e as 'Imagens'
           -> IO Picture -- ^ Mostra na tela a junção de todas as imagens
--desenhar modo playable
printState (Playable (Jogo mapa j:proximosniveis,save) fundo jogador c, img)= return $ Translate (fromIntegral(length (head mapa))/(-2)*50+25)  (fromIntegral(length mapa)/2*50-25)  $ pictures [translate (fromIntegral(length (head mapa))/2*50-25)  (fromIntegral(length mapa)/(-2)*50+25) $ scale 0.835 0.835 $ fromJust $ lookup "jogo1" $ backgraunds img,fundo, jogador]

--desenhar modo multiplayer
--printState (Multiplayer ((Jogo' mapa j1 j2:proximosniveis,save),fundo,jogadorUm,jogadorDois,c,_), img)= return $ Translate ((fromIntegral(length (head mapa))/(-2))*50+25) ((fromIntegral(length mapa)/2)*50-25) $ pictures [translate ((fromIntegral(length (head mapa))/2)*50-25)  ((fromIntegral(length mapa)/(-2))*50+25) $ scale 0.835 0.835 $ fromJust $ lookup "jogo1" $ backgraunds img,fundo, jogadorUm, jogadorDois]
printState (Multiplayer (Jogo' mapa j1 j2:proximosniveis,save) fundo jogadorUm jogadorDois c _, img)= return $ Translate (fromIntegral(length (head mapa))/(-2)*50+25) (fromIntegral(length mapa)/2*50-25) $ pictures [translate (fromIntegral(length (head mapa))/2*50-25)  (fromIntegral(length mapa)/(-2)*50+25) $ scale 4.5 4.5 $ fromJust $ lookup "jogo2" $ backgraunds img,fundo, jogadorUm, jogadorDois]

--desenhar Menu
printState (Menu (2,1) s n _, img) |s == "Normal" = return $ Pictures [scale 0.8 0.8 $ fromJust $ lookup "Menu" $ backgraunds img , translate (-10) (-75) $ scale 0.4 0.4 $ fromJust $ lookup n $ botoes img, translate (-10) 35 $ scale 0.45 0.45 $ fromJust $ lookup (-4) $ botoes img, translate (-10) (-185) $ scale 0.4 0.4 $ fromJust $ lookup (-6) $ botoes img,polygon [(180,0),(180,70),(220,35),(180,0)]]
                                   |otherwise     = return $ Pictures [scale 0.8 0.8 $ fromJust $ lookup "Menu" $ backgraunds img , translate (-10) (-75) $ scale 0.4 0.4 $ fromJust $ lookup n $ botoes img, translate (-10) 35 $ scale 0.45 0.45 $ fromJust $ lookup (-5) $ botoes img, translate (-10) (-185) $ scale 0.4 0.4 $ fromJust $ lookup (-6) $ botoes img,polygon [(-200,0),(-200,70),(-240,35),(-200,0)]]
printState (Menu (2,2) s n _, img) |s == "Normal" && n==1 = return $ Pictures [scale 0.8 0.8 $ fromJust $ lookup "Menu" $ backgraunds img , translate (-10) (-75) $ scale 0.45 0.45 $ fromJust $ lookup n $ botoes img, translate (-10) 35 $ scale 0.4 0.4 $ fromJust $ lookup (-4) $ botoes img, translate (-10) (-185) $ scale 0.4 0.4 $ fromJust $ lookup (-6) $ botoes img,polygon [(180,-110),(180,-40),(220,-75),(180,-110)]]
                                   |s == "Double" && n==1 = return $ Pictures [scale 0.8 0.8 $ fromJust $ lookup "Menu" $ backgraunds img , translate (-10) (-75) $ scale 0.45 0.45 $ fromJust $ lookup n $ botoes img, translate (-10) 35 $ scale 0.4 0.4 $ fromJust $ lookup (-5) $ botoes img, translate (-10) (-185) $ scale 0.4 0.4 $ fromJust $ lookup (-6) $ botoes img,polygon [(180,-110),(180,-40),(220,-75),(180,-110)]]
                                   |s == "Normal" && n>1 && n < nMapaNormal = return $ Pictures [scale 0.8 0.8 $ fromJust $ lookup "Menu" $ backgraunds img , translate (-10) (-75) $ scale 0.45 0.45 $ fromJust $ lookup n $ botoes img, translate (-10) 35 $ scale 0.4 0.4 $ fromJust $ lookup (-4) $ botoes img, translate (-10) (-185) $ scale 0.4 0.4 $ fromJust $ lookup (-6) $ botoes img,polygon [(-200,-110),(-200,-40),(-240,-75),(-200,-110)],polygon [(180,-110),(180,-40),(220,-75),(180,-110)]]
                                   |s == "Double" && n>1 && n < nMapaDouble = return $ Pictures [scale 0.8 0.8 $ fromJust $ lookup "Menu" $ backgraunds img , translate (-10) (-75) $ scale 0.45 0.45 $ fromJust $ lookup n $ botoes img, translate (-10) 35 $ scale 0.4 0.4 $ fromJust $ lookup (-5) $ botoes img, translate (-10) (-185) $ scale 0.4 0.4 $ fromJust $ lookup (-6) $ botoes img,polygon [(-200,-110),(-200,-40),(-240,-75),(-200,-110)],polygon [(180,-110),(180,-40),(220,-75),(180,-110)]]
                                   |s == "Normal" && n == nMapaNormal = return $ Pictures [scale 0.8 0.8 $ fromJust $ lookup "Menu" $ backgraunds img, translate (-10) (-75) $ scale 0.45 0.45 $ fromJust $ lookup n $ botoes img, translate (-10) 35 $ scale 0.4 0.4 $ fromJust $ lookup (-4) $ botoes img, translate (-10) (-185) $ scale 0.4 0.4 $ fromJust $ lookup (-6) $ botoes img,polygon [(-200,-110),(-200,-40),(-240,-75),(-200,-110)]]
                                   |s == "Double" && n == nMapaDouble = return $ Pictures [scale 0.8 0.8 $ fromJust $ lookup "Menu" $ backgraunds img, translate (-10) (-75) $ scale 0.45 0.45 $ fromJust $ lookup n $ botoes img, translate (-10) 35 $ scale 0.4 0.4 $ fromJust $ lookup (-5) $ botoes img, translate (-10) (-185) $ scale 0.4 0.4 $ fromJust $ lookup (-6) $ botoes img,polygon [(-200,-110),(-200,-40),(-240,-75),(-200,-110)]]
printState (Menu (2,3) s n _, img) |s == "Normal"= return $ Pictures [scale 0.8 0.8 $ fromJust $ lookup "Menu" $ backgraunds img, translate (-10) (-75) $ scale 0.4 0.4 $ fromJust $ lookup n $ botoes img, translate (-10) 35 $ scale 0.4 0.4 $ fromJust $ lookup (-4) $ botoes img, Color blue (translate (-10) (-185) $ scale 0.45 0.45 $ fromJust $ lookup (-6) $ botoes img)]
                                   |otherwise    = return $ Pictures [scale 0.8 0.8 $ fromJust $ lookup "Menu" $ backgraunds img, translate (-10) (-75) $ scale 0.4 0.4 $ fromJust $ lookup n $ botoes img, translate (-10) 35 $ scale 0.4 0.4 $ fromJust $ lookup (-5) $ botoes img, Color blue (translate (-10) (-185) $ scale 0.45 0.45 $ fromJust $ lookup (-6) $ botoes img)]

printState (Menu (1,1) _ _ _, img) = return $ Pictures [scale 0.8 0.8 (fromJust $ lookup "Menu" $ backgraunds img), translate (-10) (-75) $ scale 0.4 0.4 $ fromJust $ lookup (-2) $ botoes img, translate (-10) 35 $ scale 0.45 0.45 $ fromJust $ lookup (-1) $ botoes img, translate (-10) (-185) $ scale 0.4 0.4 $ fromJust $ lookup (-3) $ botoes img]
printState (Menu (1,2) _ _ _, img) = return $ Pictures [scale 0.8 0.8 (fromJust $ lookup "Menu" $ backgraunds img), translate (-10) (-75) $ scale 0.45 0.45 $ fromJust $ lookup (-2) $ botoes img, translate (-10) 35 $ scale 0.4 0.4 $ fromJust $ lookup (-1) $ botoes img, translate (-10) (-185) $ scale 0.4 0.4 $ fromJust $ lookup (-3) $ botoes img]
printState (Menu (1,3) _ _ _, img) = return $ Pictures [scale 0.8 0.8 (fromJust $ lookup "Menu" $ backgraunds img), translate (-10) (-75) $ scale 0.4 0.4 $ fromJust $ lookup (-2) $ botoes img, translate (-10) 35 $ scale 0.4 0.4 $ fromJust $ lookup (-1) $ botoes img, translate (-10) (-185) $ scale 0.45 0.45 $ fromJust $ lookup (-3) $ botoes img]

--desenhar MenuPause
printState (Pause 1 Playable {}, img)    = return $ Pictures [fromJust (lookup "Pause" $ backgraunds img), translate (-350) 0 (scale 0.4 0.4 $ fromJust (lookup "ControlSingle" $ backgraunds img)), translate 10 35 $ scale 0.45 0.45 $ fromJust $ lookup (-2) $ botoes img, translate 10 (-105) $ scale 0.4 0.4 $ fromJust $ lookup (-8) $ botoes img]
printState (Pause 2 Playable {}, img)    = return $ Pictures [fromJust (lookup "Pause" $ backgraunds img), translate (-350) 0 (scale 0.4 0.4 $ fromJust (lookup "ControlSingle" $ backgraunds img)), translate 10 35 $ scale 0.4 0.4 $ fromJust $ lookup (-2) $ botoes img, translate 10 (-105) $ scale 0.45 0.45 $ fromJust $ lookup (-8) $ botoes img]
printState (Pause 1 Multiplayer {}, img) = return $ Pictures [fromJust (lookup "Pause" $ backgraunds img), translate (-350) (-75) (scale 1 1 $ fromJust (lookup "ControlMult" $ backgraunds img)), translate 10 35 $ scale 0.45 0.45 $ fromJust $ lookup (-7) $ botoes img, translate 10 (-105) $ scale 0.4 0.4 $ fromJust $ lookup (-8) $ botoes img]
printState (Pause 2 Multiplayer {}, img) = return $ Pictures [fromJust (lookup "Pause" $ backgraunds img), translate (-350) (-75) (scale 1 1 $ fromJust (lookup "ControlMult" $ backgraunds img)), translate 10 35 $ scale 0.4 0.4 $ fromJust $ lookup (-7) $ botoes img, translate 10 (-105) $ scale 0.45 0.45 $ fromJust $ lookup (-8) $ botoes img]
--debug
printState _ = error "algo de errado não está certo (T5 printState)"

menuPauseP = [Translate (-40) (-20) (Scale 0.30 0.30 (Text "Load")),Translate (-85) (-135) (Scale 0.30 0.30 (Text "MainMenu"))]
menuPauseM = [Translate (-72) (-20) (Scale 0.30 0.30 (Text "Resume")),Translate (-85) (-135) (Scale 0.30 0.30 (Text "MainMenu"))]
{- | A função reageTecla, como o ome sugere, reage a uma Tecla e altera o estado de uma forma determinada:

        - Nos menus as setas servem para navegar, alteram um dos valores dependendo de para onde se mexe e o Enter executa o que dado menu indica, isto é:

                - pode avançar/voltar nos menus,
                - voltar aonde tinha gravado a ultima vez,
                - começar um novo jogo onde se segue todos os niveis 1 a 1 (tanto no singleplayer como no multiplayer ),
                - fazer 1 nivel escolhido

        - No SinglePlayer as setas e o a/s/w/d são equivalentes a movimentos e existem teclas com funcionalidades a parte como o T para gravar, o Esc para parar e o R para reiniciar
        - No multiplayer estão dadas a cada um dos conjuntos um jogador e também usofruem das teclas R e Esc

O singleplayer foi relativamente simples como a unica coisa que alterava a imagem de fundo era o 'InterageCaixa' foi posto de parte porem os outros foram todos juntos numa função.

O único problema era fazer o jogador entrar na porta pas isso foi conseguido com uma restrição e com alterações na tarefa 4.

@

--Jogo SinglePlayer
reageTecla (EventKey a Down _ _) (Playable (j:nextLevels,save) fundo _ c@(col,l), img) |b == InterageCaixa && j /= nJ = return (Playable  (nJ:nextLevels,save) (Pictures (mapaToPicture img mapa (0,0))) imgJogador c, img)
                                             |b /= Nenhum && (coord ==(col,l-1) || coord ==(col,l)) = return ('novoMapa' ((j:nextLevels,save), img))
                                             |b /= Nenhum && j /= nJ = return (Playable (nJ:nextLevels,save) fundo imgJogador c, img)
                    where (b,_) = traduzEvento a
                          nJ@ (Jogo mapa jogador@(Jogador coord _ _)) = 'moveJogador' j b
                          imgJogador = pictureJogador img jogador

reageTecla (EventKey key Down _ _) (stt@ Playable {}, img) | key == Char 'p' || key == SpecialKey KeyEsc = return  (Pause 1 stt, img)
reageTecla (EventKey (Char 'r') Down _ _) (Playable (_:restartlevel@(Jogo mapa jogador):restoDosNiveis,save)  _  _ c, img) = return (Playable (restartlevel:restartlevel:restoDosNiveis,save) (Pictures (mapaToPicture img mapa (0,0))) (pictureJogador img jogador) c, img)
reageTecla (EventKey (Char 't') Down _ _) (Playable (faseAtual,_) fundo jogador c, img) = do writeFile "Load.text" (show faseAtual)
                                                                                             return (Playable (faseAtual,faseAtual) fundo jogador c, img)
@

O multiplayer foi mais complicado de se fazer pois, ao contrario do singleplayer, não se puderia usar a função 'moveJogador' diretamente dado que tinha dois jogadores,
para o fazermos tivemos que auxiliarmente chamar apenas o que queriamos (nj1 e nj2) e, com elas, devolver um novo jogo, mas com isso surgiu um outro problema, tinhamos dois jogadores e interações entre eles teriam de existir.

Um exemplo disso é quando o jogador é esmagado pela caixa que o outro largou, tivemos então de fazer restrições para expecificamente o 'InterageCaixa'.

Depois queriamos que os jogadores entrassem na porta mas tal não era possivel fizemos alterações na tarefa 4 e ent fizemos 3 restrições uma para cada jogador e uma para quando ambos entravam.

Por último queriamos que o jogo continua-se depois de um jogador entrar na porta, e esse jogador não se puderia mexer. Fizemos entrão mais duas restrições(uma para cada jogador).

@

--Jogo MultiPlayer
reageTecla (EventKey a Down _ _) start@(Multiplayer (j@(Jogo' m j1'@(Jogador coord1'@(colj1,linj1) _ _) j2'@(Jogador coord2'@(colj2,linj2) _ _)):nextLevels,save) fundo j1 j2 c@(col,l) boxs, img)
                  |n == 1 && (elem coord1' boxs || coord1' == c) || n == 2 && (elem coord2' boxs ||coord2' == c) = return start--- quando está com uma caixa por cima(nao se pode mexer) 

                  |n == 1 && coord2' == c && (coord1 == (col,l-1) || coord1 == c) = return $ 'novoMapa'' ((j:nextLevels,save), img)--- entram na porta
                  |n == 2 && coord1' == c && (coord2 == (col,l-1) || coord2 == c) = return $ 'novoMapa'' ((j:nextLevels,save), img)--- entram na porta

                  |b == (InterageCaixa,1) && elem coord2' (caixas map1) = return (Multiplayer (nJ1:nextLevels,save) (Pictures (mapaToPicture img map1 (0,0))) imgJogador1 j2Esmagar c (caixas map1), img)  ---pega/larga caixa mas o outro jogador está a frente (esmaga-o) ou interage uma outra caixa enquanto está esmagado
                  |b == (InterageCaixa,1) && coord2' == c = return (Multiplayer (nJ1:nextLevels,save) (Pictures (mapaToPicture img map1 (0,0))) imgJogador1 j2 c (caixas map1), img)                       ---pega/larga caixa pega numa caixa mas o outro jogador já acabou 
                  |b == (InterageCaixa,1) = return (Multiplayer (nJ1:nextLevels,save) (Pictures (mapaToPicture img map1 (0,0))) imgJogador1 ('pictureJogador'' img j2') c (caixas map1), img)              ---pega/larga caixa normal 

                  |b == (InterageCaixa,2) && elem coord1' (caixas map2) = return (Multiplayer (nJ2:nextLevels,save) (Pictures (mapaToPicture img map2 (0,0))) j1Esmagar imgJogador2 c (caixas map2), img)  ---pega/larga caixa mas o outro jogador está a frente (esmaga-o) ou interage uma outra caixa enquanto está esmagado
                  |b == (InterageCaixa,2) && coord1' == c = return (Multiplayer (nJ2:nextLevels,save) (Pictures (mapaToPicture img map2 (0,0))) j1 imgJogador2 c (caixas map2), img)                       ---pega/larga caixa pega numa caixa mas o outro jogador já acabou 
                  |b == (InterageCaixa,2) = return (Multiplayer (nJ2:nextLevels,save) (Pictures (mapaToPicture img map2 (0,0))) ('pictureJogador' img j1') imgJogador2 c (caixas map2), img)               ---pega/larga caixa normal

                  |n == 1 && (coord1 == (col,l-1) || coord1 == c) = return (Multiplayer (Jogo' map1 (Jogador c Este False) j2' :nextLevels,save) fundo fimJogador j2 c boxs, img) -- um dos jogadores ja acabou
                  |n == 2 && (coord2 == (col,l-1) || coord2 == c) = return (Multiplayer (Jogo' map2 j1' (Jogador c Este False) :nextLevels,save) fundo j1 fimJogador c boxs, img) -- um dos jogadores ja acabou

                  |n == 1 = return (Multiplayer (nJ1:nextLevels,save) fundo imgJogador1 j2 c boxs, img) --move jogador1 como se fosse singleplayer
                  |n == 2 = return (Multiplayer (nJ2:nextLevels,save) fundo j1 imgJogador2 c boxs, img) --move jogador2 como se fosse singleplayer
                  where b@(mov,n) = traduzEvento a
                        nJ1@ (Jogo' map1 jog1'@(Jogador coord1 _ _) _) = Jogo' nMapa nJogador j2'
                          where posMovimento@(Jogo nMapa nJogador) = 'moveJogador' (Jogo m j1') mov
                        nJ2@ (Jogo' map2 _ jog2'@(Jogador coord2 _ _)) = Jogo' nMapa j1' nJogador
                          where posMovimento@(Jogo nMapa nJogador) = 'moveJogador' (Jogo m j2') mov
                        imgJogador1 = 'pictureJogador'img jog1'
                        imgJogador2 = 'pictureJogador'' img jog2'
                        j2Esmagar = translate (fromIntegral(50*colj2)) (fromIntegral (-50*linj2-1)) $ scale 0.089 0.09 $ fromJust $ lookup "esmagarJ2" $ jogador' img
                        j1Esmagar = translate (fromIntegral(50*colj1)) (fromIntegral (-50*linj1-1)) $ scale 0.089 0.09 $ fromJust $ lookup "esmagarJ1" $ jogador' img
                        fimJogador = Translate (fromIntegral col*50+5) (fromIntegral (-l)*50) (scale 0.09 0.095 $ fromJust $ lookup Porta $ pecas img)

@

-}
reageTecla :: Event -- ^ Recebe uma tecla do teclado ou rato
           -> World -- ^ Recebe também também o estádo antes da tecla ter sido primida
           -> IO World -- ^ Caso a tecla esteja definida para esse estado altera-o, caso contrário devolve o mesmo estado
--MainMenu
reageTecla (EventKey (SpecialKey KeyEnter) Down _ _) (Menu (1,2) _ _ save@(Jogo mapa jogador:restoDosNiveis), img) = return (Playable (save,save) (Pictures (mapaToPicture img mapa (0,0))) (pictureJogador img jogador) (coordPorta mapa), img)
reageTecla (EventKey (SpecialKey KeyEnter) Down _ _) (Menu (1,3) _ _ a, img) = exitSuccess
reageTecla (EventKey (SpecialKey KeyEnter) Down _ _) (Menu (1,1) a b c, img) = return (Menu (2,1) a b c, img)
reageTecla (EventKey (SpecialKey key) Down _ _) (Menu (m,opc) a b c, img) | key == KeyUp && opc > 1 = return (Menu (m,opc-1) a b c, img)
                                                                          | key == KeyDown && opc < 3 = return (Menu (m,opc+1) a b c, img)
--ModeMenu
reageTecla (EventKey (SpecialKey KeyRight) Down _ _) (Menu (2,1) "Normal" _ a, img) = return (Menu (2,1) "Double" 1 a, img)
reageTecla (EventKey (SpecialKey KeyLeft) Down _ _)  (Menu (2,1) "Double" _ a, img) = return (Menu (2,1) "Normal" 1 a, img)
reageTecla (EventKey (SpecialKey KeyEnter) Down _ _) (Menu (2,1) "Double" _ a, img) = return (Multiplayer (levelMaps,a) (Pictures (mapaToPicture img mapa (0,0))) (pictureJogador img jog1) (pictureJogador' img jog2) (coordPorta mapa) (caixas mapa), img)
    where levelMaps@(Jogo' mapa jog1 jog2:restoDosNiveis) = allMultiMaps 1
reageTecla (EventKey (SpecialKey KeyEnter) Down _ _) (Menu (2,1) "Normal" _ a, img) = return (Playable (levelMaps,levelMaps) (Pictures (mapaToPicture img mapa (0,0))) (pictureJogador img jogador) (coordPorta mapa), img)
    where levelMaps@(Jogo mapa jogador:restoDosNiveis)= allGameMaps 1
reageTecla (EventKey (SpecialKey KeyRight) Down _ _) (Menu (2,2) "Normal" n a, img) | n < nMapaNormal = return (Menu (2,2) "Normal" (n+1) a, img)
reageTecla (EventKey (SpecialKey KeyRight) Down _ _) (Menu (2,2) "Double" n a, img) | n < nMapaDouble = return (Menu (2,2) "Double" (n+1) a, img)
reageTecla (EventKey (SpecialKey KeyLeft) Down _ _) (Menu (2,2) str n a, img)       |n > 1            = return (Menu (2,2) str (n-1) a, img)
reageTecla (EventKey (SpecialKey KeyEnter) Down _ _) (Menu  (2,2) "Normal" n a, img) = return (Playable ([nivel,nivel],a) (Pictures (mapaToPicture img mapa (0,0))) (pictureJogador img jogador) (coordPorta mapa), img)
    where nivel@(Jogo mapa jogador)= gameMap n
reageTecla (EventKey (SpecialKey KeyEnter) Down _ _) (Menu (2,2) "Double" n a, img) = return (Multiplayer ([nivel,nivel],a) (Pictures (mapaToPicture img mapa (0,0))) (pictureJogador img jog1) (pictureJogador' img jog2) (coordPorta mapa) (caixas mapa), img)
    where nivel@(Jogo' mapa jog1 jog2)= multiMap n
reageTecla (EventKey (SpecialKey KeyEnter) Down _ _) (Menu (2,3) c b a, img) = return (Menu (1,1) "Normal" 1 a, img)

--PauseMenu
reageTecla (EventKey (SpecialKey KeyEnter) Down _ _) (Pause 1 (Playable(_,save@(Jogo mapa jogador: restoDosNiveis)) _ _ c), img) = return (Playable (save,save) (Pictures (mapaToPicture img mapa (0,0))) (pictureJogador img jogador) (coordPorta mapa), img)
reageTecla (EventKey (SpecialKey KeyEnter) Down _ _) (Pause 2 (Playable(_,save) _ _ _), img) = return (Menu (1,1) "Normal" 1 save, img)
reageTecla (EventKey (SpecialKey KeyEnter) Down _ _) (Pause 2 (Multiplayer (_,save) _ _ _ _ _), img) = return (Menu (1,1) "Normal" 1 save, img)
reageTecla (EventKey (SpecialKey KeyDown) Down _ _) (Pause 1 a, img) = return (Pause 2 a, img)
reageTecla (EventKey (SpecialKey KeyUp) Down _ _) (Pause 2 a, img) = return (Pause 1 a, img)
reageTecla (EventKey key Down _ _) (Pause n a, img) | key == Char 'p' || key == SpecialKey KeyEsc || n == 1 && key == SpecialKey KeyEnter= return (a, img)

--Jogo SinglePlayer
reageTecla (EventKey a Down _ _) (Playable (j:nextLevels,save) fundo _ c@(col,l), img) |b == InterageCaixa && j /= nJ = return (Playable  (nJ:nextLevels,save) (Pictures (mapaToPicture img mapa (0,0))) imgJogador c, img)
                                             |b /= Nenhum && (coord ==(col,l-1) || coord ==(col,l)) = return (novoMapa ((j:nextLevels,save), img))
                                             |b /= Nenhum && j /= nJ = return (Playable (nJ:nextLevels,save) fundo imgJogador c, img)
                    where (b,_) = traduzEvento a
                          nJ@ (Jogo mapa jogador@(Jogador coord _ _)) = moveJogador j b
                          imgJogador = pictureJogador img jogador

reageTecla (EventKey key Down _ _) (stt@ Playable {}, img) | key == Char 'p' || key == SpecialKey KeyEsc = return  (Pause 1 stt, img)
reageTecla (EventKey (Char 'r') Down _ _) (Playable (_:restartlevel@(Jogo mapa jogador):restoDosNiveis,save)  _  _ c, img) = return (Playable (restartlevel:restartlevel:restoDosNiveis,save) (Pictures (mapaToPicture img mapa (0,0))) (pictureJogador img jogador) c, img)
reageTecla (EventKey (Char 't') Down _ _) (Playable (faseAtual,_) fundo jogador c, img) = do writeFile "Load.text" (show faseAtual)
                                                                                             return (Playable (faseAtual,faseAtual) fundo jogador c, img)

--Jogo MultiPlayer
reageTecla (EventKey a Down _ _) start@(Multiplayer (j@(Jogo' m j1'@(Jogador coord1'@(colj1,linj1) _ _) j2'@(Jogador coord2'@(colj2,linj2) _ _)):nextLevels,save) fundo j1 j2 c@(col,l) boxs, img)
                  |n == 1 && (elem coord1' boxs || coord1' == c) || n == 2 && (elem coord2' boxs ||coord2' == c) = return start--- quando está com uma caixa por cima(nao se pode mexer) e quando está com uma caixa por cima(nao se pode mexer)

                  |n == 1 && coord2' == c && (coord1 == (col,l-1) || coord1 == c) = return $ novoMapa' ((j:nextLevels,save), img)--- entram na porta
                  |n == 2 && coord1' == c && (coord2 == (col,l-1) || coord2 == c) = return $ novoMapa' ((j:nextLevels,save), img)--- entram na porta

                  |b == (InterageCaixa,1) && elem coord2' (caixas map1) = return (Multiplayer (nJ1:nextLevels,save) (Pictures (mapaToPicture img map1 (0,0))) imgJogador1 j2Esmagar c (caixas map1), img)---pega/larga caixa
                  |b == (InterageCaixa,1) && coord2' == c = return (Multiplayer (nJ1:nextLevels,save) (Pictures (mapaToPicture img map1 (0,0))) imgJogador1 j2 c (caixas map1), img)---pega/larga caixa
                  |b == (InterageCaixa,1) = return (Multiplayer (nJ1:nextLevels,save) (Pictures (mapaToPicture img map1 (0,0))) imgJogador1 (pictureJogador' img j2') c (caixas map1), img)---pega/larga caixa

                  |b == (InterageCaixa,2) && elem coord1' (caixas map2) = return (Multiplayer (nJ2:nextLevels,save) (Pictures (mapaToPicture img map2 (0,0))) j1Esmagar imgJogador2 c (caixas map2), img)---pega/larga caixa
                  |b == (InterageCaixa,2) && coord1' == c = return (Multiplayer (nJ2:nextLevels,save) (Pictures (mapaToPicture img map2 (0,0))) j1 imgJogador2 c (caixas map2), img)---pega/larga caixa
                  |b == (InterageCaixa,2) = return (Multiplayer (nJ2:nextLevels,save) (Pictures (mapaToPicture img map2 (0,0))) (pictureJogador img j1') imgJogador2 c (caixas map2), img)---pega/larga caixa

                  |n == 1 && (coord1 == (col,l-1) || coord1 == c) = return (Multiplayer (Jogo' map1 (Jogador c Este False) j2' :nextLevels,save) fundo fimJogador j2 c boxs, img)
                  |n == 2 && (coord2 == (col,l-1) || coord2 == c) = return (Multiplayer (Jogo' map2 j1' (Jogador c Este False) :nextLevels,save) fundo j1 fimJogador c boxs, img)

                  |n == 1 = return (Multiplayer (nJ1:nextLevels,save) fundo imgJogador1 j2 c boxs, img)
                  |n == 2 = return (Multiplayer (nJ2:nextLevels,save) fundo j1 imgJogador2 c boxs, img)
                  where b@(mov,n) = traduzEvento a
                        nJ1@ (Jogo' map1 jog1'@(Jogador coord1 _ _) _) = Jogo' nMapa nJogador j2'
                          where posMovimento@(Jogo nMapa nJogador) = moveJogador (Jogo m j1') mov
                        nJ2@ (Jogo' map2 _ jog2'@(Jogador coord2 _ _)) = Jogo' nMapa j1' nJogador
                          where posMovimento@(Jogo nMapa nJogador) = moveJogador (Jogo m j2') mov
                        imgJogador1 = pictureJogador img jog1'
                        imgJogador2 = pictureJogador' img jog2'
                        j2Esmagar = translate (fromIntegral(50*colj2)) (fromIntegral (-50*linj2-1)) $ scale 0.089 0.09 $ fromJust $ lookup "esmagarJ2" $ jogador' img
                        j1Esmagar = translate (fromIntegral(50*colj1)) (fromIntegral (-50*linj1-1)) $ scale 0.089 0.09 $ fromJust $ lookup "esmagarJ1" $ jogador' img
                        fimJogador = Translate (fromIntegral col*50+5) (fromIntegral (-l)*50) (scale 0.09 0.095 $ fromJust $ lookup Porta $ pecas img)

reageTecla (EventKey (Char 'r') Down _ _) (Multiplayer (_:restartlevel@(Jogo' mapa jog1 jog2):restoDosNiveis,save) _ _ _ c _, img) = return (Multiplayer (restartlevel:restartlevel:restoDosNiveis,save) (Pictures (mapaToPicture img mapa (0,0))) (pictureJogador img jog1) (pictureJogador' img jog2) (coordPorta mapa) (caixas mapa), img)
reageTecla (EventKey key Down _ _) (stt@ Multiplayer {}, img) | key == Char 'p' || key == SpecialKey KeyEsc = return  (Pause 1 stt, img)

reageTecla _ a = return a

traduzEvento :: Key -> (Movimento,Int)
traduzEvento (SpecialKey KeyRight) = (AndarDireita,2)
traduzEvento (SpecialKey KeyLeft) = (AndarEsquerda,2)
traduzEvento (SpecialKey KeyUp) = (Trepar,2)
traduzEvento (SpecialKey KeyDown) = (InterageCaixa,2)
traduzEvento (Char 'd') = (AndarDireita,1)
traduzEvento (Char 'a') = (AndarEsquerda,1)
traduzEvento (Char 'w') = (Trepar,1)
traduzEvento (Char 's') = (InterageCaixa,1)
traduzEvento (Char 'D') = (AndarDireita,1)
traduzEvento (Char 'A') = (AndarEsquerda,1)
traduzEvento (Char 'W') = (Trepar,1)
traduzEvento (Char 'S') = (InterageCaixa,1)
traduzEvento _ = (Nenhum,3)

--coord das caixas no mapa
caixas :: Mapa -> [Coordenadas]
caixas m = takecoords $ filter caixa (desconstroiMapa m)
    where caixa (c,_) = c == Caixa
          takecoords [] = []
          takecoords ((_,c):resto) = c : takecoords resto

-- | muda para o próximo mapa da lista e quando acaba volta ao menu inicial
novoMapa :: (([Jogo],[Jogo]),Imagens) -> World
novoMapa (([_,_],save), img) = (Menu (1,1) "Normal" 1 save, img)
novoMapa ((_:_:restoDosNiveis@(proximoLevel@(Jogo mapa jogador):nextLevels),save), img) = (Playable (proximoLevel:nextLevels,save) (Pictures (mapaToPicture img mapa (0,0))) (pictureJogador img jogador) (coordPorta mapa), img)
novoMapa (_, img) = (inicialState, img)

-- | muda para o próximo mapa quando da lista e quando acaba volta ao menu inicial 
novoMapa' :: (([Jogo'],[Jogo]),Imagens) -> World
novoMapa' (([_,_],save), img) = (Menu (1,1) "Normal" 1 save, img)
novoMapa' ((_:_:restoDosNiveis@(proximoLevel@(Jogo' mapa jog1 jog2):nextLevels),save), img) = (Multiplayer (proximoLevel:nextLevels,save) (Pictures (mapaToPicture img mapa (0,0))) (pictureJogador img jog1) (pictureJogador' img jog2) (coordPorta mapa) (caixas mapa), img)
novoMapa' (_, img) = (inicialState, img)

time :: Float -> World -> IO World
time _  = return
{- |
Função main do Gloss 
Os p(n) são imagens em formato .bmp importadas da pasta bmps utiliza as funções 

@
main :: IO ()
main = do
    let estado = (inicialState,Imagens [(Bloco,p3),(Caixa,p4),(Porta,p5)] [(Oeste,p1),(Este,p2)] [("Oeste",p7),("Este",p6),("esmagarJ1",p8),("esmagarJ2",p9),("bracosAoArE",p26),("bracosAoArO",p27)] [("jogo1",p10),("jogo2",p11),("Menu",p12),("Pause",p28),("ControlSingle",p30),("ControlMult",p29)] [(-1,p13),(-2,p14),(-3,p15),(-4,p16),(-5,p17),(-6,p18),(-8,p31),(-7,p32),(1,p19),(2,p20),(3,p21),(4,p22),(5,p23),(6,p24),(7,p25)])
    playIO
        FullScreen
        (light $ light blue)
        30
        estado
        'printState'
        'reageTecla'
        time

@
-}
main :: IO ()
main = do
    p1 <- loadBMP "bmps/spidermanO.bmp"
    p2 <- loadBMP "bmps/spidermanE.bmp"
    p3 <- loadBMP "bmps/parede.bmp"
    p4 <- loadBMP "bmps/caixa.bmp"
    p5 <- loadBMP "bmps/portal.bmp"
    p6 <- loadBMP "bmps/2spidermanE.bmp"
    p7 <- loadBMP "bmps/2spidermanO.bmp"
    p8 <- loadBMP "bmps/esmagadoJ1.bmp"
    p9 <- loadBMP "bmps/esmagadoJ2.bmp"
    p10 <- loadBMP "bmps/background3.bmp"
    p11 <- loadBMP "bmps/background2.bmp"
    p12 <- loadBMP "bmps/bgMenu.bmp"
    p13 <- loadBMP "bmps/new_game2.bmp"
    p14 <- loadBMP "bmps/load.bmp"
    p15 <- loadBMP "bmps/exit.bmp"
    p16 <- loadBMP "bmps/single.bmp"
    p17 <- loadBMP "bmps/multi.bmp"
    p18 <- loadBMP "bmps/back.bmp"
    p19 <- loadBMP "bmps/level1.bmp"
    p20 <- loadBMP "bmps/level2.bmp"
    p21 <- loadBMP "bmps/level3.bmp"
    p22 <- loadBMP "bmps/level4.bmp"
    p23 <- loadBMP "bmps/level5.bmp"
    p24 <- loadBMP "bmps/level6.bmp"
    p25 <- loadBMP "bmps/level7.bmp"
    p26 <- loadBMP "bmps/SCCE.bmp"
    p27 <- loadBMP "bmps/SCCO.bmp"
    p28 <- loadBMP "bmps/bgPause.bmp"
    p29 <- loadBMP "bmps/controlsMult.bmp"
    p30 <- loadBMP "bmps/Controls.bmp"
    p31 <- loadBMP "bmps/mainmenu.bmp"
    p32 <- loadBMP "bmps/resume.bmp"

    let estado = (inicialState,Imagens [(Bloco,p3),(Caixa,p4),(Porta,p5)] [(Oeste,p1),(Este,p2)] [("Oeste",p7),("Este",p6),("esmagarJ1",p8),("esmagarJ2",p9),("bracosAoArE",p26),("bracosAoArO",p27)] [("jogo1",p10),("jogo2",p11),("Menu",p12),("Pause",p28),("ControlSingle",p30),("ControlMult",p29)] [(-1,p13),(-2,p14),(-3,p15),(-4,p16),(-5,p17),(-6,p18),(-8,p31),(-7,p32),(1,p19),(2,p20),(3,p21),(4,p22),(5,p23),(6,p24),(7,p25)])
    playIO
        FullScreen
        (light $ light blue)
        30
        estado
        printState
        reageTecla
        time