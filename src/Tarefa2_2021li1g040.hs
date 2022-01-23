{- |
Module      : Tarefa2_2021li1g040
Description : Construção/Desconstrução do mapa
Copyright   : Daniel da Silva Pereira <a100545@alunos.uminho.pt>;
            : Rodrigo Viana Ramos Casal Novo <a100534@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.

= Construção e desconstrução do mapa

A função __@constroiMapa@__ recebe numa lista de pecas, ordenada ou não, e retorna um mapa,
ou seja, uma lista de listas de pecas, em que cada lista de pecas corresponde a uma linha do mapa.
A função __@desconstroiMapa@__ recebe um mapa e torna-a numa lista de peças. 
-}
module Tarefa2_2021li1g040 where
import LI12122
import Tarefa1_2021li1g040
{- |
@
constroiMapa :: [(Peca, Coordenadas)] -> Mapa                 
constroiMapa input_mapa | validaPotencialMapa input_mapa = constroiMapaT4 input_mapa
                        | otherwise = []
@
A função @constroiMapa@ recorre uma função auxiliar @controiMapaT4@ pois, caso recorressemos à função @constroiMapa@
na tarefa 4, esta poderia falhar devido à validação do mapa. Sendo assim, a verificação do mapa é feita apenas 
em @constroiMapa@ procedendo para a construção do mapa em @constroiMapaT4@ (caso o @validaMapa@ retorne True).
-}

constroiMapa :: [(Peca, Coordenadas)] -- ^ Recebe a lista de pecas, ordenadas ou não
             -> Mapa                  -- ^ Retorna o mapa construido
constroiMapa m | validaPotencialMapa m = constroiMapaT4 m
               | otherwise = []
-- | desconstroi mapa da T4
constroiMapaT4 :: [(Peca, Coordenadas)] -> Mapa 
constroiMapaT4 input_mapa = constroi_aux (0,0) (ordena_linha_coluna [] input_mapa input_mapa (nLinhas input_mapa)) [] (tamanhoMapa input_mapa)
    where 
          constroi_aux :: (Int,Int) -> [(Peca, Coordenadas)] -> [Peca] -> Coordenadas -> Mapa
          constroi_aux (c,l) [] mapa (numcul,numlin) 
            |c == numcul +1 = concat_map mapa (0,0) (numcul,numlin) []
            | otherwise = constroi_aux (c+1,l) [] (mapa ++ [Vazio]) (numcul,numlin)          
          constroi_aux (c,l) pecas@((p1,(c1,l1)):ss) mapa (numcul,numlin) | (c<=numcul) && (l<=numlin) && isCoordIn (c,l) pecas = constroi_aux (c+1,l) ss (mapa ++ [p1]) (numcul,numlin)
                                                                          | (c<=numcul) && (l<=numlin) = constroi_aux (c+1,l) pecas (mapa ++ [Vazio]) (numcul,numlin)
                                                                          | otherwise = constroi_aux (0,l+1) pecas mapa (numcul,numlin)
          
          concat_map :: [Peca] -> (Int,Int) -> (Int,Int) -> [Peca] -> Mapa
          concat_map [] (accCol,accLin) (cColunas,cLinhas) juntador = []
          concat_map (h:t) (accCol,accLin) (cColunas,cLinhas) juntador | accCol < cColunas && accLin <= cLinhas = concat_map t (accCol +1 ,accLin) (cColunas,cLinhas) (juntador++[h])
                                                                       | accCol == cColunas && accLin <= cLinhas = (juntador ++ [h]) : (concat_map t (0 ,accLin+1) (cColunas,cLinhas) [])
                                                                       | otherwise = []
          
{-| A função @ordena_linha_coluna@ ordena o mapa por linhas e consecutivamente por colunas,
recorrendo posteriormente à função @ordenaColuna@ para o fazer retornando assim uma lista de peças 
ordenada tornando mais facil a execução da função @constroiMapa@
 -}
ordena_linha_coluna :: [(Peca,Coordenadas)] -- ^ Recebe um acumulador 
                    -> [(Peca,Coordenadas)] -- ^ Recebe a lista de pecas que serao verificas
                    -> [(Peca,Coordenadas)] -- ^ Recebe a mesma lista de peças que nunca será alterado, servindo apenas para repor a lista de peças anterior quando a função passa para uma nova linha
                    -> Int                  -- ^ Recebe o número de linhas do mapa
                    -> [(Peca,Coordenadas)] -- ^ Retorna a lista de peças ordenada
ordena_linha_coluna acc [] mapa linha 
    | linha < 0 = acc 
    | otherwise = (ordena_linha_coluna [] mapa mapa (linha-1)) ++ (ordenaColuna acc)

ordena_linha_coluna acc ((p,(c,l)):ss) mapa linha  
    | l == linha = ordena_linha_coluna (acc ++ [(p,(c,l))]) ss mapa linha 
    | otherwise = ordena_linha_coluna acc ss mapa linha 

{-| 
A função @ordenaColuna@ funciona similarmente ao quicksort mas ligeiramente adaptado ao problema em questão recorrendo às 
funções __@maisPequenos@__ e __@maiores@__
-}
ordenaColuna :: [(Peca,Coordenadas)] -- ^ Recebe a lista de peças ordenada por linhas
             -> [(Peca,Coordenadas)] -- ^ Retorna a lista de peças ordenada por linhas e por colunas
ordenaColuna [(p1,(c1,l1))] = [(p1,(c1,l1))]
ordenaColuna [] = []
ordenaColuna ((p1,(c1,l1)):ss) = (ordenaColuna (maisPequenos c1 ss)) ++ [(p1,(c1,l1))] ++ (ordenaColuna (maiores c1 ss))

{-| Coleciona as pecas com a coluna mais pequena em comparação com a do elemento central da função @ordenaColuna@ -}
maisPequenos :: Int -> [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
maisPequenos _ []= []
maisPequenos c ((p1,(c1,l1)):ss) | c1 <= c = (p1,(c1,l1)) : maisPequenos c ss
                                 | otherwise = maisPequenos c ss
 
{-| Coleciona as pecas com a coluna maior em comparação com a do elemento central da função @ordenaColuna@ -}
maiores :: Int -> [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]        
maiores _ [] = []
maiores c ((p1,(c1,l1)):ss) | c1 > c = (p1,(c1,l1)) : maiores c ss
                            | otherwise = maiores c ss 
 
{-| 
 A @isCoordin@ verifica se a coordenada em questão consta no input 
-}
isCoordIn :: Coordenadas -> [(Peca, Coordenadas)] -> Bool
isCoordIn _ [] = False
isCoordIn (c1,l1) ((a,b):ss) = (c1,l1) == b || isCoordIn (c1,l1) ss

{-| Calcula o número de linhas do mapa-}
nLinhas :: [(Peca, Coordenadas)] -> Int
nLinhas mapa = l 
    where (_,l) = tamanhoMapa mapa
 
-----------------------------------------
{-|
A função __@desconstroiMapa@__ recebe uma lista de lista de peças e retorna uma lista de peças com as suas respetivas coordenadas 
ignorando os espaços vazios.
-}
desconstroiMapa :: Mapa -- ^ Recebe o mapa
                -> [(Peca, Coordenadas)] -- ^ Retorna uma lista de peças com as respetivas coordenadas
desconstroiMapa l = desconstroiMapa' l (0,0) 
-- | auxiliar da desconstroi mapa 
desconstroiMapa' :: Mapa -> (Int,Int) -> [(Peca, Coordenadas)]
desconstroiMapa' [] (_,_)  = []
desconstroiMapa' ([] : resto) (c,l)  = desconstroiMapa' resto (0, l+1) 
desconstroiMapa' ([Vazio] : resto) (c,l) = desconstroiMapa' ([]:resto) (c, l) 
desconstroiMapa' ((peca1:pecas) : resto) (c,l) | peca1 == Vazio = desconstroiMapa' (pecas : resto) (c+1, l) 
                                               | otherwise = (peca1, (c,l)) : desconstroiMapa' (pecas : resto) (c+1, l)  
                                               