-- I1M 2015-16: Relación 35 (30 de abril de 2016)
-- Implementación del TAD de los grafos mediante listas.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================
 
-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------
 
-- El objetivo de esta relación es implementar el TAD de los grafos
-- mediante listas, de manera análoga a las implementaciones estudiadas
-- en el tema 22 que se encuentran en 
--    http://www.cs.us.es/~jalonso/cursos/i1m-15/temas/tema-22.html
-- y usando la mismas signatura.
 
-- ---------------------------------------------------------------------
-- Signatura                                                          --
-- ---------------------------------------------------------------------
 
module Rel_35
    (Orientacion (..),
     Grafo,
     creaGrafo,  -- (Ix v,Num p) => Orientacion -> (v,v) -> [(v,v,p)] -> 
                 --                 Grafo v p
     dirigido,   -- (Ix v,Num p) => (Grafo v p) -> Bool
     adyacentes, -- (Ix v,Num p) => (Grafo v p) -> v -> [v]
     nodos,      -- (Ix v,Num p) => (Grafo v p) -> [v]
     aristas,    -- (Ix v,Num p) => (Grafo v p) -> [(v,v,p)]
     aristaEn,   -- (Ix v,Num p) => (Grafo v p) -> (v,v) -> Bool
     peso        -- (Ix v,Num p) => v -> v -> (Grafo v p) -> p
    ) where
 
-- ---------------------------------------------------------------------
-- Librerías auxiliares                                               --
-- ---------------------------------------------------------------------
 
import Data.Array
import Data.List
 
-- ---------------------------------------------------------------------
-- Representación de los grafos mediante listas                       --
-- ---------------------------------------------------------------------
 
-- Orientacion es D (dirigida) ó ND (no dirigida).
data Orientacion = D | ND
                   deriving (Eq, Show)
 
-- (Grafo v p) es un grafo con vértices de tipo v y pesos de tipo p.
data Grafo v p = G Orientacion ([v],[((v,v),p)])
                 deriving (Eq, Show)
 
-- ---------------------------------------------------------------------
-- Ejercicios                                                         --
-- ---------------------------------------------------------------------
 
-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    creaGrafo :: (Ix v, Num p) => Bool -> (v,v) -> [(v,v,p)] -> Grafo v p
-- tal que (creaGrafo o cs as) es un grafo (dirigido o no, según el
-- valor de o), con el par de cotas cs y listas de aristas as (cada
-- arista es un trío formado por los dos vértices y su peso). Por
-- ejemplo, 
--    ghci> creaGrafo ND (1,3) [(1,2,12),(1,3,34)]
--    G ND ([1,2,3],[((1,2),12),((1,3),34),((2,1),12),((3,1),34)])
--    ghci> creaGrafo D (1,3) [(1,2,12),(1,3,34)]
--    G D ([1,2,3],[((1,2),12),((1,3),34)])
--    ghci> creaGrafo D (1,4) [(1,2,12),(1,3,34)]
--    G D ([1,2,3,4],[((1,2),12),((1,3),34)])
-- ---------------------------------------------------------------------
 
creaGrafo :: (Ix v, Num p) => 
              Orientacion -> (v,v) -> [(v,v,p)] -> Grafo v p
creaGrafo o cs vs =
    G o (range cs, [((a,b),c) | (a,b,c) <- vs]
                   ++ (if o == D then []
                   else [((b,a),c) | (a,b,c) <- vs]))
 
-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir, con creaGrafo, la constante 
--    ejGrafoND :: Grafo Int Int 
-- para representar el siguiente grafo no dirigido
--             12
--        1 -------- 2
--        | \78     /|
--        |  \   32/ |
--        |   \   /  |
--      34|     5    |55
--        |   /   \  |
--        |  /44   \ |
--        | /     93\|
--        3 -------- 4
--             61
--    ghci> ejGrafoND
--    G ND ([1,2,3,4,5],
--          [((1,2),12),((1,3),34),((1,5),78),((2,4),55),((2,5),32),
--           ((3,4),61),((3,5),44),((4,5),93),((2,1),12),((3,1),34),
--           ((5,1),78),((4,2),55),((5,2),32),((4,3),61),((5,3),44),
--           ((5,4),93)])
-- ---------------------------------------------------------------------
 
ejGrafoND :: Grafo Int Int 
ejGrafoND = creaGrafo ND (1,5) [(1,2,12),(1,3,34),(1,5,78),
                                (2,4,55),(2,5,32),
                                (3,4,61),(3,5,44),
                                (4,5,93)]
 
-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir, con creaGrafo, la constante 
--    ejGrafoD :: Grafo Int Int 
-- para representar el grafo anterior donde se considera que las aristas
-- son los pares (x,y) con x < y. Por ejemplo,
--    ghci> ejGrafoD
--    G D ([1,2,3,4,5],
--         [((1,2),12),((1,3),34),((1,5),78),((2,4),55),((2,5),32),
--          ((3,4),61),((3,5),44),((4,5),93)])
-- ---------------------------------------------------------------------
 
ejGrafoD :: Grafo Int Int
ejGrafoD = creaGrafo D (1,5) [(1,2,12),(1,3,34),(1,5,78),
                              (2,4,55),(2,5,32),
                              (3,4,61),(3,5,44),
                              (4,5,93)]
 
-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    dirigido :: (Ix v,Num p) => (Grafo v p) -> Bool
-- tal que (dirigido g) se verifica si g es dirigido. Por ejemplo,
--    dirigido ejGrafoD   ==  True
--    dirigido ejGrafoND  ==  False
-- ---------------------------------------------------------------------
 
dirigido :: (Ix v,Num p) => (Grafo v p) -> Bool
dirigido (G o _) = o == D
 
-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    nodos :: (Ix v,Num p) => (Grafo v p) -> [v]
-- tal que (nodos g) es la lista de todos los nodos del grafo g. Por
-- ejemplo, 
--    nodos ejGrafoND  ==  [1,2,3,4,5]
--    nodos ejGrafoD   ==  [1,2,3,4,5]
-- ---------------------------------------------------------------------
 
nodos :: (Ix v,Num p) => (Grafo v p) -> [v]
nodos (G _ (ns,_)) = ns
 
-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    adyacentes :: (Ix v, Num p) => Grafo v p -> v -> [v]
-- tal que (adyacentes g v) es la lista de los vértices adyacentes al
-- nodo v en el grafo g. Por ejemplo,
--    adyacentes ejGrafoND 4  ==  [5,2,3]
--    adyacentes ejGrafoD  4  ==  [5]
-- ---------------------------------------------------------------------
 
adyacentes :: (Ix v, Num p) => Grafo v p -> v -> [v]
adyacentes (G _ (_,as)) v = [x | ((a,x),_) <- as, a == v]
 
-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    aristaEn :: (Ix v,Num p) => Grafo v p -> (v,v) -> Bool
-- (aristaEn g a) se verifica si a es una arista del grafo g. Por
-- ejemplo,
--    aristaEn ejGrafoND (5,1)  ==  True
--    aristaEn ejGrafoND (4,1)  ==  False
--    aristaEn ejGrafoD  (5,1)  ==  False
--    aristaEn ejGrafoD  (1,5)  ==  True
-- ---------------------------------------------------------------------
 
aristaEn :: (Ix v,Num p) => Grafo v p -> (v,v) -> Bool
aristaEn (G _ (_,as)) = flip elem (map fst as)
 
-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    peso :: (Ix v,Num p) => v -> v -> Grafo v p -> p
-- tal que (peso v1 v2 g) es el peso de la arista que une los vértices
-- v1 y v2 en el grafo g. Por ejemplo,
--    peso 1 5 ejGrafoND  ==  78
--    peso 1 5 ejGrafoD   ==  78
-- ---------------------------------------------------------------------
 
peso :: (Ix v,Num p) => v -> v -> Grafo v p -> p
peso a b (G _ (_,as)) = head [p | ((x,y),p) <- as, a == x , b == y]
 
-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    aristas :: (Ix v,Num p) => Grafo v p -> [(v,v,p)]
-- (aristasD g) es la lista de las aristas del grafo g. Por ejemplo, 
--    ghci> aristas ejGrafoD
--    [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),
--     (3,5,44),(4,5,93)] 
--    ghci> aristas ejGrafoND
--    [(1,2,12),(1,3,34),(1,5,78),(2,1,12),(2,4,55),(2,5,32),
--     (3,1,34),(3,4,61),(3,5,44),(4,2,55),(4,3,61),(4,5,93),
--     (5,1,78),(5,2,32),(5,3,44),(5,4,93)]
-- ---------------------------------------------------------------------
 
aristas :: (Ix v,Num p) => Grafo v p -> [(v,v,p)]
aristas (G _ (_,as)) = [(x,y,p) | ((x,y),p) <- as]
