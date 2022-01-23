{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{- |
Module      : Tarefa1_2021li1g040
Description : Validação de um potencial mapa
Copyright   : Daniel da Silva Pereira <a100545@alunos.uminho.pt>;
            : Rodrigo Viana Ramos Casal Novo <a100534@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g040 where

import LI12122


{- | 
= Validação do Mapa
-}

{- | 
Para a validarção de um possivel mapa temos em conta cinco parametros sendo que, se algum deles tiver valor negativo, esse mapa será vetado.
(a validação não necessita a organização dos termos).

A função 'validaPotencialMapa' é definida da seguinte forma:

@
validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False
validaPotencialMapa l = validaPosicao l && validaChao l && validaPorta l && validaPorta' r && validaCaixa l && validaEspacosVazios r
    where r = pura l (cMaisBaixos (percursoChao l))

@

E, com ela, validamos então:

        - A posição de cada peça (não haver sobreposição de peças) com a função 'validaPosicao'; 

        - A existencia de uma única porta (e esta ser a cima do chão)com as funções 'validaPorta' e 'validaPorta'';

        - A ausência de caixas a flutuar com a função 'validaCaixa'; 

        - Se há algum espaço vazio no mapa com a função 'validaEspacosVazios';

        - Se o mapa tem um chão completo com a função 'validaChao'.

-}
validaPotencialMapa :: [(Peca, Coordenadas)] -- ^ Recebe um mapa 
                    -> Bool -- ^ Diz se o pudemos usar
validaPotencialMapa [] = False
validaPotencialMapa l = validaPosicao l && validaChao l && validaCaixa l && validaPorta l && validaPorta' r && validaEspacosVazios r
    where r = foldr purifica l (cMaisBaixos (percursoChao l)) --sem nd a baixo do chão
          purifica :: Coordenadas -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
          purifica _ [] = []
          purifica (c1,l1) ((peca,(c2,l2)):resto) | c1 == c2 && l1 < l2 = (Bloco,(c2,l2)) : purifica (c1,l1) resto
                                                  | otherwise = (peca,(c2,l2)) : purifica (c1,l1) resto

          cMaisBaixos ::[Coordenadas] -> [Coordenadas]
          cMaisBaixos [] = []
          cMaisBaixos [a] = [a]
          cMaisBaixos ((c1,l1): (c2,l2) : xs) | (c1 < c2) || ((c1==c2) && (l1 < l2)) = (c1,l1) : cMaisBaixos ((c2,l2) : xs)
                                              | otherwise = cMaisBaixos ((c1,l1) : xs)
-- | Coordenadas por onde passa o chão 
percursoChao :: [(Peca,Coordenadas)] -- ^ Recebe o mapa
             -> [Coordenadas] -- ^ devolve as coordenadas por onde passa o chão
percursoChao l | c1 == 0 = listadoChao 0 (c1,l1) [] (listaBlocos l) (cu,lu)
               | otherwise = []
            where (c1,l1) = findFrstChao t h
                  (cu,lu) = findLastChao t h
                  (h:t) = listaBlocos l
{- | Semelhante á função 'verifCont' pois utiliza as funções 'habloco', 'eomaisbaixo', 'listaBlocos', 
sendo a única exceção a devolução de um mapa ao invês de um booliano.
-}
-- verifica se o chao mais baixo da primeira coluna está lidado á segunda coluna
listadoChao :: Int -- ^ Recebe uma direção
            -> Coordenadas -- ^ Recebe as coordenadas do chão atual
            -> [Coordenadas] -- ^ Recebe uma lista vazia que serve de acomulador
            -> [Coordenadas] -- ^ Recebe a lista de coordenadas dos blocos do mapa
            -> Coordenadas -- ^ Recebe as coordenadas do ultimo bloco
            -> [Coordenadas] -- ^ Devolve a lista de blocos por onde passa o chão
listadoChao 2 (c,_) acc [] (x,_) | c == x = acc
                               | otherwise = []
listadoChao _ (c,l) acc [] (x,y) | c == x && l == y = acc
                               | otherwise = []
listadoChao a (c,l) acc list u |a == 0 && eomaisbaixo (c,l) list && habloco (c+1,l) list = listadoChao 0 (c+1,l)  (acc ++ [(c,l)]) list u
                             |a == 0 && eomaisbaixo (c,l) list && habloco (c,l-1) list = listadoChao 2 (c,l-1) (acc ++ [(c,l)]) list u
                             |a == 1 && eomaisbaixo (c,l) list && habloco (c+1,l) list = listadoChao 0 (c+1,l) (acc ++ [(c,l)]) list u
                             |a == 2 && habloco (c+1,l) list = listadoChao 0 (c+1,l) (acc ++ [(c,l)]) list u
                             |a == 2 && habloco (c,l-1) list = listadoChao 2 (c,l-1) (acc ++ [(c,l)]) list u
                             |a == 1 && habloco (c,l+1) list = listadoChao 1 (c,l+1) (acc ++ [(c,l)]) list u
                             |a == 0 && habloco (c,l+1) list = listadoChao 1 (c,l+1) (acc ++ [(c,l)]) list u
                             |a == 0 && habloco (c+1,l+1) list = listadoChao 0 (c+1,l+1) (acc ++ [(c,l)]) list u
                             |a == 0 && habloco (c+1,l-1) list = listadoChao 0 (c+1,l-1) (acc ++ [(c,l)]) list u
                             |a == 1 && habloco (c+1,l+1) list = listadoChao 0 (c+1,l+1) (acc ++ [(c,l)]) list u
                             |a == 2 && habloco (c+1,l-1) list = listadoChao 0 (c+1,l-1) (acc ++ [(c,l)]) list u
                             |otherwise = listadoChao a (c,l) (acc ++ [(c,l)]) [] u

----------------- 1 -----------------
{- |
== Validação das Posições
Para validar a posição comparamos, com uma função auxiliar, o primeiro termo com todos os outros (com a função recursiva validaCabeca) e, caso algum seja igual, devolve False. 

Caso o termo não se repita, repete-se o procedimento para o termo asseguir.  

@
validaPosicao :: [(Peca, Coordenadas)] -> Bool
validaPosicao [(_,_)] = True 
validaPosicao (termo : resto) = validaCabeca (termo : resto) && validaPosicao resto

validaCabeca [(p, (coluna1 , linha1))] = True 
validaCabeca ((p, (coluna1 , linha1)) : (_, (coluna2 , linha2)) : resto') = (coluna1 '/=' coluna2 || linha1 '/=' linha2) && validaCabeca((p, (coluna1 , linha1)) : resto')

@

Com isto verificamos se existem elementos em posições repetidas.
-}
validaPosicao :: [(Peca, Coordenadas)] -- ^ Recebe o mapa
                 -> Bool -- ^ Devolve False se dois termos estiverem na mesma posição 
validaPosicao [(_,_)] = True -- Método de terminar a validação (se todos os termos forem diferentes devolve True)
validaPosicao (termo : resto) = validaCabeca (termo : resto) && validaPosicao resto
    where
    validaCabeca [(p, (coluna1 , linha1))] = True -- Método de terminar a validação (depois de comparar com o último termo termina)
    validaCabeca ((p, (coluna1 , linha1)) : (_, (coluna2 , linha2)) : resto') = (coluna1 /= coluna2 || linha1 /= linha2) && validaCabeca((p, (coluna1 , linha1)) : resto')

{-
--para mapa ordenado
validaPosicao :: [(Peca, Coordenadas)] -> Bool
validaPosicao [(_,_)] = True
validaPosicao ((_, (coluna1 , linha1)) : (p, (coluna2 , linha2)) : resto)
    = (coluna1 /= coluna2 || linha1 /= linha2) || validaPosicao ((p, (coluna2 , linha2)) : resto)
-}

----------------- 2 -----------------

{- |
== Validação da Porta
Para validar as portas, começamos por correr a função até ocorrer a primeira porta, se tal não acontecer devolve False.

Caso aconteça é ativado uma segunda função chamada validaPorta' que continua a correr a função até encontrar uma segunda 
porta, se encontrar devolve False e se acabar o mapa devolve True.

Depois ainda é corrida uma função para verificar se a porta que existe está acessível

@
validaPorta :: [(Peca, Coordenadas)] -> Bool
validaPorta [] = False
validaPorta ((p,_) : resto) | p /= Porta = validaPorta resto
                            | otherwise = validaPorta'' resto
    where
    validaPorta'' :: [(Peca, Coordenadas)] -> Bool
    validaPorta'' [] = True
    validaPorta'' ((p,_) : resto) =  p /= Porta && validaPorta'' resto
@
-}
validaPorta :: [(Peca, Coordenadas)] -- ^ Recebe o mapa 
               -> Bool -- ^ Devolve True se existir uma única porta  
validaPorta [] = False
validaPorta ((p,_) : resto) | p /= Porta = validaPorta resto
                            | otherwise = validaPorta'' resto
    where
    validaPorta'' :: [(Peca, Coordenadas)] -> Bool
    validaPorta'' [] = True
    validaPorta'' ((p,_) : resto) =  p /= Porta && validaPorta'' resto
{- |
@
validaPorta' :: [(Peca, Coordenadas)] -> Bool
validaPorta' [] = False
validaPorta'  ((p,_) : resto) | p /= Porta = validaPorta' resto
                              | otherwise = validaPorta'' resto
    where
    validaPorta'' :: [(Peca, Coordenadas)] -> Bool
    validaPorta'' [] = True
    validaPorta'' ((p,_) : resto) =  p /= Porta && validaPorta'' resto
@

Com isto validamos a porta.
-}
validaPorta' :: [(Peca, Coordenadas)] -- ^ Recebe o mapa com o chão coberto até à última linha
             -> Bool -- ^ devolve True se ainda existir porta
validaPorta' [] = False
validaPorta'  ((p,_) : resto) | p /= Porta = validaPorta' resto
                              | otherwise = validaPorta'' resto
    where
    validaPorta'' :: [(Peca, Coordenadas)] -> Bool
    validaPorta'' [] = True
    validaPorta'' ((p,_) : resto) =  p /= Porta && validaPorta'' resto
----------------- 3 -----------------
{- |
== Validação das caixas
Não é necissária a existência de Caixas num mapa, porém, é necessário um chão por baixo delas.

Tendo isso em conta, procede-se então com a função validaCaixa' que precorre o mapa em busca de uma caixa, se esta não existir
não há problemas (devolve True).

Caso exista, com a função validaSubCaixa, vai-se procurar no mapa dado, a base da caixa, se esta existir e não for uma porta devolverá True. 

@
validaCaixa :: [(Peca, Coordenadas)] -> Bool
validaCaixa l = validaCaixa' l l
    where
    validaCaixa' :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> Bool
    validaCaixa' [] _= True
    validaCaixa' ((peca,(coluna,linha)) : ss) mapa | peca == Caixa = validaSubCaixa (coluna,linha + 1) mapa && validaCaixa' ss mapa
                                                   | otherwise = validaCaixa' ss mapa
        where
        validaSubCaixa :: Coordenadas -> [(Peca, Coordenadas)] -> Bool
        validaSubCaixa (_,_) [] = False
        validaSubCaixa (col1, line1) ((peca,(col2, line2)): ss) 
                        | col1 == col2 && line1 == line2 && (peca == Caixa || peca == Bloco) = True
                        | otherwise = validaSubCaixa (col1, line1) ss

@

Com isto validamos as possiveis caixas.
-}

validaCaixa :: [(Peca, Coordenadas)]  -- ^ Recebe um mapa 
               -> Bool -- ^ Devolve True se não existirem caixas ou se, as que existirem tiverem um Bloco/Caixa por baixo
validaCaixa l = validaCaixa' l l
    where
    validaCaixa' :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> Bool
    validaCaixa' [] _= True
    validaCaixa' ((peca,(coluna,linha)) : ss) mapa | peca == Caixa = validaSubCaixa (coluna,linha + 1) mapa && validaCaixa' ss mapa
                                                   | otherwise = validaCaixa' ss mapa
        where
        validaSubCaixa :: Coordenadas -> [(Peca, Coordenadas)] -> Bool
        validaSubCaixa (_,_) [] = False
        validaSubCaixa (col1, line1) ((peca,(col2, line2)): ss) = col1 == col2 && line1 == line2 && (peca == Caixa || peca == Bloco) || validaSubCaixa (col1, line1) ss

{-
-- supondo que só se recebe listas ordenadas

validaCaixa :: [(Peca, Coordenadas)] -> Bool
validaCaixa [(Caixa, _)] = False
validaCaixa [_] = True
validaCaixa [] = True
validaCaixa ((p1,(c1,l1)) : peca@(p2,(c2,l2)) : ss) | p1 == Caixa && (c1 /= c2 || l1+1 /= l2 || p2 == Porta) = False
                                                    | otherwise = validaCaixa (peca : ss)
-}

----------------- 4 -----------------

{- |
== Verificação da existência de espaços vazios
Para o mapa ser usável é necessário que haja pelo menos um sitio onde colocar a personagem.

Para isso utilizamos a função 'validaEspacosVazios' que compara o espaço ocupado por peças com o tamanho escalar do mapa
caso tal espaço seja igual ou superior devolve False.

@

validaEspacosVazios l = validaEspacosVazios' l (tamanhoMapa l)
    where
    validaEspacosVazios' :: [(Peca, Coordenadas)] -> (Int,Int) -> Bool
    validaEspacosVazios' p (c,l) = elemVazios p || espacoOcupado < espacosTotais
        where espacoOcupado = length p
              espacosTotais = (c+1)*(l+1)
              elemVazios [] = False
              elemVazios ((pecas, (_, _)) : xs) | pecas == Vazio = True
                                                | otherwise = elemVazios xs

@

Com isto verificamos se o mapa é jogável.
-}

validaEspacosVazios :: [(Peca, Coordenadas)] -- ^ Recebe o mapa com a baixo do chão coberto de blocos
                    -> Bool-- ^ Devolve True se o mapa tiver pelo menos um espaço vazio e False se tal não acontecer
validaEspacosVazios l = validaEspacosVazios' l (tamanhoMapa l)
    where
    validaEspacosVazios' :: [(Peca, Coordenadas)] -> (Int,Int) -> Bool
    validaEspacosVazios' p (c,l) = elemVazios p || length p < (c+1)*(l+1)
        where elemVazios [] = False
              elemVazios ((pecas, _) : xs) = pecas == Vazio || elemVazios xs

----------------- 5 -----------------
{- |
== Verificação da existência de um chão
Para a personagem poder percorrer todo o mapa é necessário que exista um chão para ela andar.
Verificamos tal existência com o auxilio da da função 'validaChao' que percorre a função e verifica se existe umchão continuo que passa por todas as colunas,
se tal existir devolve true.

Estes testes são exemplos:

@
testes :: Int -> Bool
testes x |x== 1 = validaChao [(Bloco,(0,3)),(Porta,(0,2)),(Bloco, (1,3)),(Bloco, (1,4)),(Bloco,(1,5)),(Bloco,(2,5)),(Bloco,(3,5)),(Bloco,(3,4)),(Bloco,(3,3)),(Bloco,(4,3)),(Bloco,(5,3))] --True
         |x== 2 = validaChao [(Bloco,(0,3)),(Porta,(0,2)),(Bloco, (0,4)),(Bloco, (1,3)),(Bloco,(1,4)),(Bloco,(1,5)),(Bloco,(2,5)),(Bloco,(3,5)),(Bloco,(3,4)),(Bloco,(3,3)),(Bloco,(4,3)),(Bloco,(5,3))] --True
         |x== 3 = validaChao [(Bloco,(0,3)),(Porta,(0,2)),(Bloco, (1,3)),(Bloco, (1,4)),(Bloco,(1,5)),(Bloco,(2,5)),(Bloco,(3,5)),(Bloco,(3,4)),(Bloco,(3,3)),(Bloco,(4,3)),(Bloco,(5,3)),(Bloco, (0,0)),(Bloco, (3,0))]--True
         |x== 4 = validaChao [(Bloco,(0,3)),(Porta,(0,2)),(Bloco, (1,3)),(Bloco, (1,4)),(Bloco,(1,5)),(Bloco,(2,5)),(Bloco,(3,5)),(Bloco,(3,4)),(Bloco,(3,3)),(Bloco,(4,3)),(Bloco,(5,3)),(Bloco, (0,0)),(Bloco, (3,0)),(Bloco,(2,6))] --False
         |x== 5 = validaChao [(Bloco,(0,0)),(Bloco,(2,0))] --False
         |x== 6 = validaChao [(Bloco,(1,1))]--False 
         |x== 7 = validaChao [(Bloco,(0,4))]--True
         |x== 8 = validaChao [(Bloco,(0,4)),(Bloco,(0,3)),(Bloco,(0,2)),(Bloco,(1,2))] --True
         |x== 9 = validaChao [(Bloco,(0,0)),(Bloco,(1,0)),(Bloco,(1,1)),(Bloco,(1,2)),(Bloco,(1,3)),(Bloco, (2,1))]--False
         |x== 0 = validaChao [(Bloco,(0,0)),(Bloco,(1,0)),(Bloco,(1,1)),(Bloco,(1,3)),(Bloco,(2,3))]--False
         |x==10 = validaChao [(Bloco,(0,3)),(Bloco, (1,3)),(Bloco, (2,3)),(Bloco, (1,4)),(Bloco,(1,5)),(Bloco,(2,5)),(Bloco,(3,5)),(Bloco,(4,4)),(Bloco,(4,3)),(Bloco,(4,5)),(Bloco,(5,3)),(Bloco, (0,0)),(Bloco, (3,0))] --True
         |x==11 = validaChao [(Bloco,(0,0)),(Bloco,(1,1)),(Bloco,(2,2)),(Bloco,(3,1)),(Bloco,(4,0))] --True

@

Com isto confirmamos por fim se existe algum boraco no mapa.

Para facilitar o processo começamos,com 'listaBlocos', por isolar as coordenadas dos Blocos.
-}

validaChao :: [(Peca,Coordenadas)] -- ^ Recebe o mapa 
           -> Bool -- ^ Verifica se existe caixas e se existir verifica se não flotuam
validaChao l = not (null (listaBlocos l)) && c1 == 0 &&  verifCont 0 (c1,l1) (listaBlocos l) (cu,lu)
    where m@(h:t) = listaBlocos l
          (c1,l1) = findFrstChao t h
          (cu,lu) = findLastChao t h


-- | coordenadas de todos os blocos
listaBlocos :: [(Peca, Coordenadas)] -- ^ Recebe o mapa completo 
            -> [Coordenadas] -- ^ Devolve uma lista com apenas as coordenadas de todos os blocos 
listaBlocos [] = []
listaBlocos ((p,(c,l)):resto) | p == Bloco = (c,l) : listaBlocos resto
                              |otherwise = listaBlocos resto
{- | 
A função verifCont recebe um inteiro que pode ser um de três e representa a direção por onde veio:

* 0 -> vem da esquerda

* 1 -> vem de cima

* 2 -> vem de baixo
-}
{- |
 Precisamos de saber em que coordenadas começar o chão e onde acabar, para isso usamos as funções que se seguem.
-}
-- verifica se o chao mais baixo da primeira coluna está lidado á segunda coluna
verifCont :: Int -- ^ De onde veio
  -> Coordenadas -- ^ Onde está de momento
  -> [Coordenadas] -- ^ coordenadas dos blocos do mapa
  -> Coordenadas -- ^ Tamanho do mapa
  -> Bool -- ^ 
verifCont 2 (c,_) [] (x,_) = c == x
verifCont _ (c,l) [] (x,y) = c == x && l == y
verifCont a (c,l) list u |a == 0 && eomaisbaixo (c,l) list && habloco (c+1,l) list = verifCont 0 (c+1,l) list u
                         |a == 0 && eomaisbaixo (c,l) list && habloco (c,l-1) list = verifCont 2 (c,l-1) list u
                         |a == 1 && eomaisbaixo (c,l) list && habloco (c+1,l) list = verifCont 0 (c+1,l) list u
                         |a == 2 && habloco (c+1,l) list = verifCont 0 (c+1,l) list u
                         |a == 2 && habloco (c,l-1) list = verifCont 2 (c,l-1) list u
                         |a == 1 && habloco (c,l+1) list = verifCont 1 (c,l+1) list u
                         |a == 0 && habloco (c,l+1) list = verifCont 1 (c,l+1) list u
                         |a == 0 && habloco (c+1,l+1) list = verifCont 0 (c+1,l+1) list u
                         |a == 0 && habloco (c+1,l-1) list = verifCont 0 (c+1,l-1) list u
                         |a == 1 && habloco (c+1,l+1) list = verifCont 0 (c+1,l+1) list u
                         |a == 2 && habloco (c+1,l-1) list = verifCont 0 (c+1,l-1) list u
                         |otherwise = verifCont a (c,l) [] u
                         -- 0 -> vem da esquerda 
                         -- 1 -> vem de cima
                         -- 2 -> vem de baixo

{- |
Função que verifica se a coordenada recebida é a mais baixa da coluna.
-}
--procura o primeiro chão 
findFrstChao :: [Coordenadas] -- ^ Recebe a lista com as coordenadas dos blocos
             -> Coordenadas -- ^ Recebe as coordenadas do primeiro bloco do ma
             -> Coordenadas -- ^ Devolve as coordenadas do bloco mais perdo do canto inferior esquerdo
findFrstChao [] acc = acc
findFrstChao ((c,l): resto) (c1,l1) | c > c1 ||c == c1 && l <= l1 = findFrstChao resto (c1,l1)
                                    | c < c1 ||c == c1 && l > l1 =  findFrstChao resto (c,l)

-- |procura o ultimo chão
findLastChao :: [Coordenadas] -- ^ Recebe a lista com as coordenadas dos blocos
             -> Coordenadas -- ^ Recebe as coordenadas do primeiro bloco do mapa
             -> Coordenadas -- ^ Devolve as coordenadas do bloco mais perdo do canto inferior direito
findLastChao [] acc = acc
findLastChao ((c,l): resto) (c1,l1) | c < c1 ||c == c1 && l <= l1 = findLastChao resto (c1,l1)
                                    | c > c1 ||c == c1 && l > l1 = findLastChao resto (c,l)

-- |verifica se é o mais baixo da coluna
eomaisbaixo :: Coordenadas -- ^ Recebe as coordenadas a verificar
            -> [Coordenadas] -- ^ Recebe a lista com as coordenadas dos blocos
            -> Bool -- ^ Verifica se a coordenada recebida é a mais baixa da coluna
eomaisbaixo _ [] = True
eomaisbaixo (cBlock,lBlock) ((c1,l1):resto) = not (c1 == cBlock && l1 > lBlock) &&  eomaisbaixo (cBlock,lBlock) resto

{- |
Função que verifica se existe um bloco na coordenada que recebe.
-}
--verifica se há direita/ há baixo/há cima/há diagonais
habloco :: Coordenadas -- ^ Recebe as coordenadas a verificar
        -> [Coordenadas] -- ^ Recebe a lista com as coordenadas dos blocos
        -> Bool -- ^ Verifica se existe um bloco na coordenada que recebe
habloco _ [] = False
habloco (c,l) ((c1,l1):resto) = c1 == c && l == l1 || habloco (c,l) resto

{- |
Para o quarto e quinto passo foi usado uma função que devolvia a escala do mapa. A essa função chamamos 'tamanhoMapa'.

@
tamanhoMapa :: [(Peca, Coordenadas)] -> (Int,Int)
tamanhoMapa  = maximo (0,0)
    where
    maximo :: (Ord a, Ord b) => (a, b) -> [(a3, (a, b))] -> (a, b)
    maximo acc [] = acc
    maximo (mc , ml) ((p1, (c1 , l1)) : resto) | mc >= c1 && ml >= l1 = maximo (mc,ml) resto
                                               | mc >= c1 && ml < l1 = maximo (mc,l1) resto
                                               | mc '<' c1 && ml '>=' l1 = maximo (c1,ml) resto
                                               | mc < c1 && ml < l1 = maximo (c1,l1) resto

@

Através desta função é nos fornecido tanto o número máximo de linhas/colunas como o número de parcelas do mapa.
-}

--tamanho do mapa
tamanhoMapa :: [(Peca, Coordenadas)] -- ^ Recebe o mapa
            -> (Int,Int) -- ^ Devolve o par (ultimaColuna, ultimaLinha)
tamanhoMapa  = maximo (0,0)
    where
    maximo :: Coordenadas -> [(Peca, Coordenadas)] -> Coordenadas
    maximo acc [] = acc
    maximo (mc , ml) ((p1, (c1 , l1)) : resto) | mc >= c1 && ml >= l1 = maximo (mc,ml) resto
                                               | mc >= c1 && ml < l1 = maximo (mc,l1) resto
                                               | mc < c1 && ml >= l1 = maximo (c1,ml) resto
                                               | mc < c1 && ml < l1 = maximo (c1,l1) resto
