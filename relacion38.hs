-- I1M 2015-16: Relación 38 (8 de mayo de 2016)
-- Ejercicios de exámenes sobre grafos.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================
 
-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------
 
-- En esta relación se presenta una recopilación de ejercicios sobre
-- grafos propuestos en exámenes de la asignatura.
-- 
-- Para realizar los ejercicios hay que tener instalada la librería I1M
-- que contiene la implementación de TAD de los polinomios. Los pasos
-- para instalarla son los siguientes:
-- + Descargar el paquete I1M desde http://bit.ly/1pbnDqm
-- + Descomprimirlo (y se crea el directorio I1M-master.zip).
-- + Cambiar al directorio I1M-master.
-- + Ejecutar cabal install I1M.cabal
-- 
-- Otra forma es descargar, en el directorio de ejercicios, la
-- implementación del TAD de grafos 
-- + GrafoConVectorDeAdyacencia que está en http://bit.ly/1SQnG4S
-- + GrafoConMatrizDeAdyacencia que está en http://bit.ly/1SQnGlB
-- 
-- Los módulos anteriores se encuentras en la página de códigos 
-- http://bit.ly/1SQnAKO
 
-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------
 
import Data.List
import Data.Array
import Test.QuickCheck
 
-- Hay que elegir una librería 
import I1M.Grafo
-- import GrafoConVectorDeAdyacencia 
-- import GrafoConMatrizDeAdyacencia 
 
-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    recorridos :: [a] -> [[a]]
-- tal que (recorridos xs) es la lista de todos los posibles recorridos
-- por el grafo cuyo conjunto de vértices es xs y cada vértice se
-- encuentra conectado con todos los otros y los recorridos pasan por
-- todos los vértices una vez y terminan en el vértice inicial. Por
-- ejemplo, 
--    ghci> recorridos [2,5,3]
--    [[2,5,3,2],[5,2,3,5],[3,5,2,3],[5,3,2,5],[3,2,5,3],[2,3,5,2]]
-- Indicación: No importa el orden de los recorridos en la lista.
-- ---------------------------------------------------------------------
 
recorridos :: [a] -> [[a]]
recorridos xs = [y:ys ++ [y] | y:ys <- permutations xs]

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Consideremos un grafo G = (V,E), donde V es un
-- conjunto finito de nodos ordenados y E es un conjunto de arcos. En un
-- grafo, la anchura de un nodo es el máximo de los valores absolutos de
-- la diferencia entre el valor del nodo y los de sus adyacentes; y la
-- anchura del grafo es la máxima anchura de sus nodos. Por ejemplo, en
-- el grafo  
--    grafo2 :: Grafo Int Int
--    grafo2 = creaGrafo ND (1,5) [(1,2,1),(1,3,1),(1,5,1),
--                                 (2,4,1),(2,5,1),
--                                 (3,4,1),(3,5,1),
--                                 (4,5,1)]
-- su anchura es 4 y el nodo de máxima anchura es el 5.
-- 
-- Definir la función 
--    anchura :: Grafo Int Int -> Int
-- tal que (anchuraG g) es la anchura del grafo g. Por ejemplo,
--    anchura g  ==  4
-- ---------------------------------------------------------------------
 
grafo2 :: Grafo Int Int
grafo2 = creaGrafo ND (1,5) [(1,2,1),(1,3,1),(1,5,1),
                             (2,4,1),(2,5,1),
                             (3,4,1),(3,5,1),
                             (4,5,1)]
 
anchura :: Grafo Int Int -> Int
anchura g = maximum [abs(x-y) | (x,y,_) <- aristas g]
 
-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Comprobar experimentalmente que la anchura del grafo
-- grafo cíclico de orden n es n-1.
-- ---------------------------------------------------------------------
 
-- La conjetura
conjetura :: Int -> Bool
conjetura n = anchura (ciclo n) == n-1
  where ciclo :: Int -> Grafo Int Int
        ciclo n = creaGrafo D (1,n) ((n,1,0):[(x,x+1,0) | x <- [1..n-1]])
 
-- La comprobación es
prop_conjetura :: (Positive Int) -> Bool
prop_conjetura (Positive n) = conjetura n
   -- λ> quickCheck prop_conjetura
   -- +++ OK, passed 100 tests.
                              
-- ---------------------------------------------------------------------
-- Ejercicio 3. Un grafo no dirigido G se dice conexo, si para cualquier
-- par de vértices u y v en G, existe al menos una trayectoria (una
-- sucesión de vértices adyacentes) de u a v.
--
-- Definirla función 
--    conexo :: (Ix a, Num p) => Grafo a p -> Bool
-- tal que (conexo g) se verifica si el grafo g es conexo. Por ejemplo, 
--    conexo (creaGrafo ND (1,3) [(1,2,0),(3,2,0)])  ==  True
--    conexo (creaGrafo ND (1,4) [(1,2,0),(3,4,0)])  ==  False
-- ---------------------------------------------------------------------
 
conexo :: (Ix a, Num p) => Grafo a p -> Bool
conexo g = length (recorridos (aristas g)) == length (nodos g)
 
-- ---------------------------------------------------------------------
--  Ejercicio 4. Un mapa se puede representar mediante un grafo donde
--  los vértices son las regiones del mapa y hay una arista entre dos
--  vértices si las correspondientes regiones son vecinas. Por ejemplo,
--  el mapa siguiente 
--        +----------+----------+       
--        |    1     |     2    |       
--        +----+-----+-----+----+       
--        |    |           |    |       
--        | 3  |     4     | 5  |       
--        |    |           |    |       
--        +----+-----+-----+----+       
--        |    6     |     7    |       
--        +----------+----------+
-- se pueden representar por
--    mapa :: Grafo Int Int
--    mapa = creaGrafo ND (1,7)
--                     [(1,2,0),(1,3,0),(1,4,0),(2,4,0),(2,5,0),(3,4,0),
--                      (3,6,0),(4,5,0),(4,6,0),(4,7,0),(5,7,0),(6,7,0)]
-- Para colorear el mapa se dispone de 4 colores definidos por   
--    data Color = A | B | C | D deriving (Eq, Show)
-- 
-- Definir la función
--    correcta :: [(Int,Color)] -> Grafo Int Int -> Bool
-- tal que (correcta ncs m) se verifica si ncs es una coloración del
-- mapa m tal que todos las regiones vecinas tienen colores distintos. 
-- Por ejemplo, 
--    correcta [(1,A),(2,B),(3,B),(4,C),(5,A),(6,A),(7,B)] mapa == True
--    correcta [(1,A),(2,B),(3,A),(4,C),(5,A),(6,A),(7,B)] mapa == False
-- ---------------------------------------------------------------------
 
mapa :: Grafo Int Int
mapa = creaGrafo ND (1,7)
                 [(1,2,0),(1,3,0),(1,4,0),(2,4,0),(2,5,0),(3,4,0),
                  (3,6,0),(4,5,0),(4,6,0),(4,7,0),(5,7,0),(6,7,0)]
 
data Color = A | B | C | E deriving (Eq, Show)
 
correcta :: [(Int,Color)] -> Grafo Int Int -> Bool
correcta ncs g = and [color x /= color y | (x,y,_) <- aristas g]
  where color :: Int -> Color
        color x = head [c | (r,c) <- ncs, r == x]
 
-- ---------------------------------------------------------------------
-- Ejercicio 5. Dado un grafo dirigido G, diremos que un nodo está
-- aislado si o bien de dicho nodo no sale ninguna arista o bien no
-- llega al nodo ninguna arista. Por ejemplo, en el siguiente grafo
-- (Tema 22, pag. 31) 
--    grafo5 = creaGrafo D (1,6) [(1,2,0),(1,3,0),(1,4,0),(3,6,0),
--                                (5,4,0),(6,2,0),(6,5,0)]
-- podemos ver que del nodo 1 salen 3 aristas pero no llega ninguna, por
-- lo que lo consideramos aislado. Así mismo, a los nodos 2 y 4 llegan
-- aristas pero no sale ninguna, por tanto también estarán aislados.
--
-- Definir la función 
--    aislados :: (Ix v, Num p) => Grafo v p -> [v]
-- tal que (aislados g) es la lista de nodos aislados del grafo g. Por
-- ejemplo, 
--    aislados grafo5 == [1,2,4]
-- ---------------------------------------------------------------------
 
grafo5 = creaGrafo D (1,6) [(1,2,0),(1,3,0),(1,4,0),(3,6,0),
                            (5,4,0),(6,2,0),(6,5,0)]
 
aislados :: (Ix v, Num p) => Grafo v p -> [v]
aislados g = filter (aislado (aristas g) True True) (nodos g)
  where aislado :: (Ix v, Num p) => [(v,v,p)] -> Bool -> Bool -> v -> Bool
        aislado ((x,y,_):as) b1 b2 n
            | x == n && y == n = False
            | x == n           = aislado as False b2    n
            |           y == n = aislado as b1    False n
            | otherwise        = aislado as b1    b2    n
        aislado []           b1 b2 _ = b1 || b2
 
-- ---------------------------------------------------------------------
-- Ejercicio 6. Consideremos una implementación del TAD de los grafos,
-- por ejemplo en la que los grafos se representan mediante listas. Un
-- ejemplo de grafo es el siguiente:
--    grafo6 :: Grafo Int Int
--    grafo6 = creaGrafo D (1,6) [(1,3,2),(1,5,4),(3,5,6),(5,1,8),(5,5,10),
--                                (2,4,1),(2,6,3),(4,6,5),(4,4,7),(6,4,9)]
-- 
-- Definir la función 
--    conectados :: Grafo Int Int -> Int -> Int -> Bool
-- tal que (conectados g v1 v2) se verifica si los vértices v1 y v2
-- están conectados en el grafo g. Por ejemplo,
--    conectados grafo6 1 3  ==  True
--    conectados grafo6 1 4  ==  False
--    conectados grafo6 6 2  ==  False
--    conectados grafo6 2 6  ==  True
-- ----------------------------------------------------------------------------
 
grafo6 :: Grafo Int Int
grafo6 = creaGrafo D (1,6) [(1,3,2),(1,5,4),(3,5,6),(5,1,8),(5,5,10),
                            (2,4,1),(2,6,3),(4,6,5),(4,4,7),(6,4,9)]
 
conectados :: Grafo Int Int -> Int -> Int -> Bool
conectados g v1 v2 = elem v2 (recorridos [v1] [])
  where recorridos :: [Int] -> [Int] -> [Int]
        recorridos [] rs = rs
        recorridos (n:ns) rs
            | n `elem` rs = recorridos rs ns
            | otherwise   = recorridos (rs++[n]) (union ns xs)
          where xs = [y | (x,y,_) <- aristas g, x == n]
