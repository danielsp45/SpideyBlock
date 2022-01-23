{- |
Module      : Tarefa4_2021li1g040
Description : Movimentação do personagem
Copyright   : Daniel da Silva Pereira <a100545@alunos.uminho.pt>;
            : Rodrigo Viana Ramos Casal Novo <a100534@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.

= Movimentos do Jogador 

A ação de movimentação de um jogador, com a função __'moveJogador'__,
baseia-se na restrição de movimentos possiveis para cada situação em que o jogador se encontra e o respetivo movimento que recebe.
Quando o o movimento aplicado é:

1.  __Movimento /AndarEsquerda/ e /AndarDireita/__: a função verifica todos os casos em que o jogador se pode encontrar
ao correr este movimento. Casos em como o jogador tem um bloco a bloquear o seu deslocamento (quando tem ou não caixa), ou quando o jogador se 
desloca de uma posicao mais alta para uma baixa obrigando-o a ter que descer para a coordenada mais abaixo possível. 
2. __Movimento /Trepar/__ : Tal como nos movimentos anteriores, a função verifica os casos em o jogador o pode fazer (quando tem ou não tem caixa).
3. __Movimento /InterageCaixa/__ : Neste caso a função 'moveJogador' vai analisar se o jogador já transporta caixa. Caso não o faça, vai verificar, 
primeiro de tudo, se existe caixa à frente do jogador e se o ambiente à sua volta permite que o jogador pegue nessa mesma caixa. 
Caso o jogador já transporte uma caixa a função vai verificar se pode pousar a caixa e vai pousa-la na coordenada mais abaixo possível (por outras palavras,
vai deixar a caixa "cair")

Estando feita a função __'moveJogador'__, para fazer a __'correrMovimentos'__ foi simplesmente preciso utilizar a recursividade com a função __'moveJogador'__.

-}

module Tarefa4_2021li1g040 where
import LI12122
import Tarefa2_2021li1g040
import Tarefa3_2021li1g040
import Tarefa1_2021li1g040

-- |Função princilal do corre movimentos
correrMovimentos :: Jogo -- ^ Recebe o jogo inicial
                -> [Movimento] -- ^ Recebe a lista de movimentos
                -> Jogo -- ^ Retorna o jogo final aplicando a lista de movimentos ao jogo
correrMovimentos jogo [] = jogo
correrMovimentos (Jogo mapa (Jogador (c,l) direcao caixa)) (movimento:ss) = correrMovimentos (moveJogador (Jogo mapa (Jogador (c,l) direcao caixa)) movimento) ss

{-| 
A função 'moveJogador recorre a uma função auxiliar 'moveJogador'' que aplica a função 'posicionaBonecoCaixa' que vai posicionar
o boneco na coordenada mais a baixo possivel de onde se encontra
-}

moveJogador :: Jogo  -- ^ Recebe o jogo
            -> Movimento -- ^ Recebe o movimento a aplicar
            -> Jogo -- ^ Retorna o jogo com o movimento aplicado 
moveJogador (Jogo mapa (Jogador (c,l) direcao caixa)) mov = posicionaFinal (moveJogador' (Jogo mapa (Jogador (posicionaBonecoCaixa mapa (c,l)) direcao caixa)) mov)
-- | Posição final
posicionaFinal :: Jogo -> Jogo
posicionaFinal (Jogo mapa (Jogador (c,l) direcao caixa)) = Jogo mapa (Jogador (posicionaBonecoCaixa mapa (c,l)) direcao caixa)
{-|
== Funções auxiliares
-}
moveJogador' :: Jogo -> Movimento -> Jogo
moveJogador' (Jogo mapa (Jogador (c,l) direcao caixa)) mov 

    | (mov == AndarEsquerda) && (verifOeste mapa' (c,l)) && (caixa == False) && (c-1 >= 0) = -- movimento==AndarEsquerda && nao tem caixa && nao tem obstrucao no caminho
        (Jogo mapa (Jogador (c-1,l) Oeste caixa))
    | (mov == AndarEsquerda) && (verifOeste mapa' (c,l)) && (caixa == False) && (c-1 >= 0) && ((posicionaBonecoCaixa mapa (c-1,l)) == porta mapa') = -- movimento==AndarEsquerda && nao tem caixa && nao tem obstrucao no caminho e vai entrar na porta
        (Jogo mapa (Jogador (c-1,l) Oeste caixa))
    | (mov == AndarEsquerda) && (caixa) && (verifCaixaOeste mapa (c,l)== False) && (c-1 >= 0)= -- movimento==AndarEsquerda && tem caixa && o caminho nao esta obstruido
        (Jogo mapa (Jogador (c-1,l) Oeste True))
    | (mov == AndarEsquerda) = -- movimento==AndarEsquerda && obstruido
        (Jogo mapa (Jogador (c,l) Oeste caixa))

    | (mov == AndarDireita) && (verifEste mapa' (c,l)) && (caixa==False) && (c+1 <= nColunas mapa')= -- movimento==AndarDireita && nao tem caixa && nao tem obstrucao no caminho
        (Jogo mapa (Jogador (c+1,l) Este caixa))
    | (mov == AndarDireita) && (verifEste mapa' (c,l)) && (caixa == False) && (c+1 <= nColunas mapa') && ((posicionaBonecoCaixa mapa (c+1,l)) == porta mapa') = -- movimento==AndarDireita && nao tem caixa && nao tem obstrucao no caminho e vai entrar na porta
        (Jogo mapa (Jogador (c+1,l) Este caixa))
    | (mov == AndarDireita) && (caixa) && (verifCaixaEste mapa (c,l)== False) && (c+1 <= nColunas mapa')= -- movimento==AndarDireita && tem caixa && o caminho nao esta obstruido
        (Jogo mapa (Jogador (c+1,l) Este True))
    | (mov == AndarDireita)= -- movimento==AndarDireita && o caminho esta obstruido
        (Jogo mapa (Jogador (c,l) Este caixa))
    
    
    | (mov == Trepar) && (direcao == Oeste) && (caixa == False) && (verifTreparOesteSemCaixa mapa (c,l)) == False && (porta mapa' == (c-1,l-1))= -- movimento==Trepar a esquerda  && nao tem caixa && nao tem o caminho obstruido e vai entrar na porta
        (Jogo mapa (Jogador (c-1,l-1) direcao caixa))
    | (mov == Trepar) && (direcao == Oeste) && (caixa == False) && (verifTreparOesteSemCaixa mapa (c,l))= -- movimento==Trepar a esquerda  && nao tem caixa && nao tem o caminho obstruido
        (Jogo mapa (Jogador (c-1,l-1) direcao caixa))
    | (mov == Trepar) && (direcao == Oeste) && (caixa == False) && (verifTreparOesteSemCaixa mapa (c,l)) && (porta mapa' == (c-1,l-1))= -- movimento==Trepar a esquerda  && nao tem caixa && nao tem o caminho obstruido e vai entrar na porta
        (Jogo mapa (Jogador (c-1,l-1) direcao caixa))
    | (mov == Trepar) && (direcao == Oeste) && (caixa) && (verifTreparOesteComCaixa mapa (c,l)) = -- movimento==Trepar a esquerda && tem caixa && nao tem o caminho obstruido
        (Jogo mapa (Jogador (c-1,l-1) direcao caixa))
    | (mov == Trepar) && (direcao == Oeste)= -- movimento==Trepar a esquerda && tem o caminho obstruido
        (Jogo mapa (Jogador (c,l) direcao caixa))


    | (mov == Trepar) && (direcao == Este) && (caixa == False) && (verifTreparEsteSemCaixa mapa (c,l)) == False && (porta mapa' == (c+1,l-1))= -- movimento==Trepar a direita  && nao tem caixa && nao tem o caminho obstruido e vai entrar na porta
        (Jogo mapa (Jogador (c+1,l-1) direcao caixa))
    | (mov == Trepar) && (direcao == Este) && (caixa == False) && (verifTreparEsteSemCaixa mapa (c,l)) = -- movimento==Trepar a direita && nao tem caixa && nao tem o caminho obstruido
        (Jogo mapa (Jogador (c+1,l-1) direcao caixa))
    | (mov == Trepar) && (direcao == Este) && caixa && (verifTreparEsteComCaixa mapa (c,l)) = -- movimento==Trepar a direita && tem caixa && nao tem o caminho obstruido
        (Jogo mapa (Jogador (c+1,l-1) direcao caixa))
    | (mov==Trepar) && (direcao == Este)= -- movimento==Trepar a direita && tem o caminho obstruido
        (Jogo mapa (Jogador (c,l) direcao caixa))

    | (mov == InterageCaixa) && (caixa==False) && (direcao==Oeste) && (verifExisteCaixaOeste mapa (c,l)) && (isCoordIn (c-1,l-1) mapa'==False) && isCoordIn (c,l-1) mapa' == False =  -- movimento==interageCaixa a esquerda && existe caixa 
        (Jogo (apanhaCaixa Oeste (c,l) mapa) (Jogador (c,l) direcao True))
    | (mov==InterageCaixa) && (caixa==False) && (direcao==Oeste) && (((verifExisteCaixaOeste mapa (c,l)) && (isCoordIn (c-1,l-1) mapa')) || (verifExisteCaixaOeste mapa (c,l) == False) || isCoordIn (c,l-1) mapa') =  -- movimento==interageCaixa a esquerda mas esta obstruido
        (Jogo mapa (Jogador (c,l) direcao caixa))

    | (mov==InterageCaixa) && (caixa==False) && (direcao==Este) && (verifExisteCaixaEste mapa (c,l)) && (isCoordIn (c+1,l-1) mapa'==False) && isCoordIn (c,l-1) mapa' == False = -- movimento==interageCaixa a direita && existe caixa
        (Jogo (apanhaCaixa Este (c,l) mapa) (Jogador (c,l) direcao True))
    | (mov==InterageCaixa) && (caixa==False) && (direcao==Este) && (((verifExisteCaixaEste mapa (c,l)) && (isCoordIn (c+1,l-1) mapa')) || (verifExisteCaixaEste mapa (c,l) == False) || isCoordIn (c,l-1) mapa') =  -- movimento==interageCaixa a direita mas esta obstruido
        (Jogo mapa (Jogador (c,l) direcao caixa))

    | (mov == InterageCaixa) && (caixa) && (direcao==Oeste) && ((isCoordIn (c-1,l-1) mapa')==False) = -- movimento==interageCaixa a esquerda && caixa==True && nao tem bloco em frente 
        (Jogo (posicionaCaixa direcao (c,l) mapa) (Jogador (c,l) direcao False))
    | (mov == InterageCaixa) && (caixa) && (direcao==Este) && ((isCoordIn (c+1,l-1) mapa')==False) = -- movimento==interageCaixa a direita && caixa==True && nao tem bloco em frente
        (Jogo (posicionaCaixa direcao (c,l) mapa) (Jogador (c,l) direcao False))
    | (mov == InterageCaixa) && (caixa) && (((isCoordIn (c-1,l-1) mapa')==True) || ((isCoordIn (c+1,l-1) mapa')==True)) = -- movimento==interageCaixa a direita && caixa==True mas o caminho esta obstruido
        (Jogo mapa (Jogador (c,l) direcao caixa))
    
    | otherwise = error "Algo correu mal"

    where mapa' = desconstroiMapa mapa
          porta :: [(Peca, Coordenadas)] -> Coordenadas  
          porta [] = error "Algo correu mal"
          porta ((p1,(c1,l1)):ss) | p1 == Porta = (c1,l1)
                                  | otherwise = porta ss


{-|Verifica se existe caixa no bloco em frente do boneco no lado Oeste-}
verifExisteCaixaOeste :: Mapa           -- ^ Recebe o mapa 
                      -> Coordenadas    -- ^ Recebe as coordenadas do jogador
                      -> Bool           -- ^ Retorna True se existir caixa
verifExisteCaixaOeste mapa (c,l) = elem (Caixa,(c-1,l)) (desconstroiMapa mapa)
-- |Verifica se existe caixa no bloco em frente do boneco no lado Este
verifExisteCaixaEste :: Mapa
                     -> Coordenadas
                     -> Bool  -- ^ Retorna True se existir caixa
verifExisteCaixaEste mapa (c,l) = elem (Caixa,(c+1,l)) (desconstroiMapa mapa)

{- |Verifca se o boneco pode trepar com caixa no sentido Este-}
verifTreparEsteComCaixa :: Mapa         -- ^ Recebe o mapa
                        -> Coordenadas  -- ^ Recebe as coordenadas do jogador
                        -> Bool         -- ^ Retorna True se poder trepar
verifTreparEsteComCaixa mapa (c,l) =  (isCoordIn (c+1,l) (desconstroiMapa mapa)) && ((isCoordIn (c+1,l-1) (desconstroiMapa mapa))==False) && ((isCoordIn (c+1,l-2) (desconstroiMapa mapa))==False) && (isCoordIn (c,l-1) (desconstroiMapa mapa))==False && (isCoordIn (c,l-2) (desconstroiMapa mapa))==False
-- |Verifica se o boneco pode trepar sem caixa nos sentido Este
verifTreparEsteSemCaixa :: Mapa -> Coordenadas -> Bool --Retorna True se poder trepar 
verifTreparEsteSemCaixa mapa (c,l) = (isCoordIn (c+1,l) (desconstroiMapa mapa)) && ((isCoordIn (c+1,l-1) (desconstroiMapa mapa))==False) && isCoordIn (c,l-1) (desconstroiMapa mapa) == False
-- |Verifca se o boneco pode trepar com caixa no sentido Oeste
verifTreparOesteComCaixa :: Mapa -> Coordenadas -> Bool -- Retorna True se poder trepar
verifTreparOesteComCaixa mapa (c,l) =  (isCoordIn (c-1,l) (desconstroiMapa mapa)) && ((isCoordIn (c-1,l-1) (desconstroiMapa mapa))==False) && ((isCoordIn (c-1,l-2) (desconstroiMapa mapa))==False) && (isCoordIn (c,l-1) (desconstroiMapa mapa))==False && (isCoordIn (c,l-2) (desconstroiMapa mapa))==False
-- |Verifica se o boneco pode trepar sem caixa nos sentido Oeste
verifTreparOesteSemCaixa :: Mapa -> Coordenadas -> Bool --Retorna True se poder trepar 
verifTreparOesteSemCaixa mapa (c,l) = (isCoordIn (c-1,l) (desconstroiMapa mapa)) && ((isCoordIn (c-1,l-1) (desconstroiMapa mapa))==False) && isCoordIn (c,l-1) (desconstroiMapa mapa) == False
 
{- | Verifica se o boenco pode andar com caixa para Oeste-}
verifCaixaOeste :: Mapa         -- ^ Recebe o mapa
                -> Coordenadas  -- ^ Recebe as coordenadas do jogador
                -> Bool         -- ^ Se tiver algo a obstruir retorna True
verifCaixaOeste mapa (c,l) = (isCoordIn (c-1,l) (desconstroiMapa mapa)) || (isCoordIn (c-1,l-1) (desconstroiMapa mapa)) || (isCoordIn (c,l-1) (desconstroiMapa mapa)) 
-- |Verifica se o boneco pode andar coma caixa para Este
verifCaixaEste :: Mapa -> Coordenadas -> Bool   --Se tiver algo a obstruir retorna True
verifCaixaEste mapa (c,l) = (isCoordIn (c+1,l) (desconstroiMapa mapa)) || (isCoordIn (c+1,l-1) (desconstroiMapa mapa)) || (isCoordIn (c,l-1) (desconstroiMapa mapa)) 

{-| Verifica se o boneco pode andar no sentido Este-}
verifEste :: [(Peca, Coordenadas)]   -- ^ Recebe o mapa
          -> Coordenadas             -- ^ Recebe as coordenadas do jogador
          -> Bool                    -- ^ Se nao tiver algo a obstruir retorna True
verifEste [] (c,l) = True
verifEste ((p1,(c1,l1)):ss) (c,l) | (p1 == Bloco || p1 == Caixa) && ((c1,l1) == (c+1,l)) = False 
                                   | otherwise = verifEste ss (c,l)
-- |Verifica se o boneco pode andar no sentido Oeste
verifOeste :: [(Peca, Coordenadas)]  -- ^ Recebe o mapa
           -> Coordenadas            -- ^ Recebe as coordenadas do jogador
           -> Bool                   -- ^ Se nao tiver algo a obstruir retorna True
verifOeste [] (c,l) = True
verifOeste ((p1,(c1,l1)):ss) (c,l) | (p1 == Bloco || p1 == Caixa) && ((c1,l1) == (c-1,l)) = False 
                                   | otherwise = verifOeste ss (c,l)

{-| Se retornar True o boneco nao pode ser inserido no mapa (se tudo funcionar bem so podera retornar True no primeio movimento)-}
verifJogador :: Mapa        -- ^ Recebe o mapa
             -> Coordenadas -- ^ Recebe as coordenadas do jogador
             -> Bool        -- ^ Retorna True se o boneco não poder ser inserido
verifJogador mapa (c,l) = isCoordIn (c,l) (desconstroiMapa mapa)


{-|Funcao para quando o boneco andar, va sempre para o bloco mais abaixo de onde andou (i.e. quando cai),
ou quando é largada uma caixa, tendo o mesmo efeito
-}
posicionaBonecoCaixa :: Mapa        -- ^ Recebe o mapa
                     -> Coordenadas -- ^ Recebe as coordenadas inicias do jogador/caixa
                     -> Coordenadas -- ^ Retorna as coordenadas do jogador/caixa posicionados no sitio certo
posicionaBonecoCaixa mapa (c,l) | (isCoordIn (c,l+1) ((p,(c1,l1)):ss)) = (c,l)
                                | otherwise = posicionaBonecoCaixa mapa (c,l+1)
    where ((p,(c1,l1)):ss) = desconstroiMapa mapa

{-|Esta funcao recebe a direcao do jogador, as suas coordenadas e o mapa e posicona a caixa na linha mais em baixo possivel para a coluna em questão-}
posicionaCaixa :: Direcao       -- ^ Recebe a direcao do jogador
               -> Coordenadas   -- ^ Recebe as coordenadas do jogador
               -> Mapa          -- ^ Recebe o mapa 
               -> Mapa          -- ^ Retorna o mapa alterado
posicionaCaixa direcao (c,l) mapa | (direcao == Oeste) && (isCoordIn (c-1,l) ((p1,(c1,l1)):ss) == False) =
                                      constroiMapaT4 ([(Caixa,(posicionaBonecoCaixa mapa (c-1,l)))] ++ ((p1,(c1,l1)):ss))
                                  | (direcao == Oeste) && (isCoordIn (c-1,l) ((p1,(c1,l1)):ss)) =
                                      constroiMapaT4 ((Caixa,(c-1,l-1)):((p1,(c1,l1)):ss))
                                  | (direcao == Este) && (isCoordIn (c+1,l) ((p1,(c1,l1)):ss) == False) =
                                      constroiMapaT4 ([(Caixa,(posicionaBonecoCaixa mapa (c+1,l)))] ++ ((p1,(c1,l1)):ss))
                                  | (direcao == Este) && (isCoordIn (c+1,l) ((p1,(c1,l1)):ss)) =
                                      constroiMapaT4 ((Caixa,(c+1,l-1)):((p1,(c1,l1)):ss))
                                  | otherwise = mapa
    where ((p1,(c1,l1)):ss) = desconstroiMapa mapa


{-|Se o jogador pegar numa caixa, verifica se a pode pegar e retira-a do mapa, caso contrario o mapa fica igual -}
apanhaCaixa :: Direcao      -- ^ Recebe a direcao do jogador 
            -> Coordenadas  -- ^ Recebe as coordenadas do jogador 
            -> Mapa         -- ^ Recebe o mapa do jogo
            -> Mapa         -- ^ Retorna o mapa alterado 
apanhaCaixa direcao (c,l) mapa | (direcao == Oeste) && (isCoordIn (c,l-1) mapa' == False) && (isCoordIn (c-1,l-1) mapa' == False) =
                                   apanhaCaixa' (c-1,l) mapa' []
                               | (direcao == Este) && (isCoordIn (c,l-1) mapa' == False) && (isCoordIn (c+1,l-1) mapa' == False) =
                                   apanhaCaixa' (c+1,l) mapa' []
                               | otherwise = mapa
    where mapa' = desconstroiMapa mapa
          
          apanhaCaixa' :: Coordenadas -> [(Peca, Coordenadas)] ->[(Peca, Coordenadas)] -> Mapa  --As primerias coordenadas sao referentes a posicao da caixa e as outras coordenadas do jogador
          apanhaCaixa' (c1,l1) [] acc = error "Ocorreu um erro na funcao apanhaCaixa"
          apanhaCaixa' (c1,l1) ((p3,(c3,l3)):ss) acc | (c1,l1) == (c3,l3) = constroiMapaT4 (ss ++ acc)
                                                     | otherwise = apanhaCaixa' (c1,l1) ss ((p3,(c3,l3)) : acc)


{-|Quando o jogador está a segurar uma caixa e ela nao esta realmente no mapa, esta funcao volta a po-la no mapa quando o caixa==True e o movimento é InterageCaixa-}
mapaLargaCaixa :: Coordenadas   -- ^ Recebe as coordenadas do jogador
               -> Mapa          -- ^ Recebe o mapa do Jogo
               -> Direcao       -- ^ Recebe a direcao 
               -> Mapa          -- ^ Retorna o mapa resultado de 'mapaLargaCaixa'' 
mapaLargaCaixa (c,l) mapa direcao | direcao == Oeste = constroiMapaT4 ((Caixa,(c-1,l)):mapa')
                                  | direcao == Este = constroiMapaT4 ((Caixa,(c+1,l)):mapa')
                                  | otherwise = error "Ocorreu um erro com mapaLargaCaixa"
    where mapa' = desconstroiMapa mapa

-- |Esta funcao faz o mesmo de a de cima so que quando o boneco tenho uma caixa/bloco em frente ent poe em cima dessa caixa/bloco 
mapaLargaCaixa' :: Coordenadas -- ^ Recebe as coordenadas do jogador
                -> Mapa        -- ^ Recebe o mapa
                -> Direcao     -- ^ Recebe a direcao do jogador 
                -> Mapa        -- ^ Retorna o mapa alterado 
mapaLargaCaixa' (c,l) mapa direcao | direcao == Oeste = constroiMapaT4 ((Caixa,(c-1,l-1)):mapa')
                                   | direcao == Este = constroiMapaT4 ((Caixa,(c+1,l-1)):mapa')
                                   | otherwise = error "Ocorreu um erro com mapaLargaCaixa'"
    where mapa' = desconstroiMapa mapa
-- | numero de colunas do mapa
nColunas :: [(Peca, Coordenadas)] -- ^ Recebe o mapa do jogo
         -> Int                   -- ^ Retorna o numero de colunas do mapa
nColunas mapa = n
    where (n,_) = tamanhoMapa mapa