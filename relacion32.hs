-- I1M 2015-16: Relación 32 (8 de abril de 2016)
-- Algoritmos de ordenación y complejidad.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación es presentar una recopilación de los
-- algoritmos de ordenación y el estudio de su complejidad.

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import qualified I1M.ColaDePrioridad as CP

-- ---------------------------------------------------------------------
-- § Ordenación por selección                                         --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Para ordenar una lista xs mediante el algoritmo de
-- ordenación por selección se selecciona el menor elemento de xs y se
-- le añade a la ordenación por selección de los restantes. Por ejemplo,
-- para ordenar la lista [3,1,4,1,5,9,2] el proceso es el siguiente:
--       ordenaPorSeleccion [3,1,4,1,5,9,2] 
--     = 1 : ordenaPorSeleccion [3,4,1,5,9,2] 
--     = 1 : 1 : ordenaPorSeleccion [3,4,5,9,2] 
--     = 1 : 1 : 2 : ordenaPorSeleccion [3,4,5,9] 
--     = 1 : 1 : 2 : 3 : ordenaPorSeleccion [4,5,9] 
--     = 1 : 1 : 2 : 3 : 4 : ordenaPorSeleccion [5,9] 
--     = 1 : 1 : 2 : 3 : 4 : 5 : ordenaPorSeleccion [9] 
--     = 1 : 1 : 2 : 3 : 4 : 5 : 9 : ordenaPorSeleccion [] 
--     = 1 : 1 : 2 : 3 : 4 : 5 : 9 : []
--     = [1,1,2,3,4,5,9]
--
-- Definir la función 
--    ordenaPorSeleccion :: Ord a => [a] -> [a]
-- tal que (ordenaPorSeleccion xs) es la lista obtenida ordenando por
-- selección la lista xs. Por ejemplo,
--    ordenaPorSeleccion [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaPorSeleccion :: Ord a => [a] -> [a]
ordenaPorSeleccion [] = []
ordenaPorSeleccion xs = x: ordenaPorSeleccion (delete x xs)
  where x = minimum xs

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaPorSeleccion [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000].
-- 
-- ¿Cuál es el orden de complejidad de ordenaPorSeleccion?
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | (0.09 secs, 67864656 bytes)
--    2000 | (0.21 secs, 291313440 bytes)
--    3000 | (0.39 secs, 679754136 bytes)
--    4000 | (0.66 secs, 1234314664 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir la función
--    ordenaPorSeleccion2 :: Ord a => [a] -> [a] 
-- tal que (ordenaPorSeleccion2 xs) es la lista xs ordenada por el
-- algoritmo de selección, pero usando un acumulador. Por ejemplo,
--    ordenaPorSeleccion2 [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaPorSeleccion2 :: Ord a => [a] -> [a] 
ordenaPorSeleccion2 [] = []
ordenaPorSeleccion2 (x:xs) = selecciona x xs []
  where selecciona :: Ord a => a -> [a] -> [a] -> [a]
        selecciona n [] [] = [n]
        selecciona n [] (y:ys) = n: selecciona y ys []
        selecciona n (x:xs) ys | n <= x    = selecciona n xs (x:ys)
                               | otherwise = selecciona x xs (n:ys)

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Calcular los tiempos necesarios para calcular 
--    let n = k in length (ordenaPorSeleccion2 [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | (0.26 secs, 95121000 bytes)
--    2000 | (0.74 secs, 410271664 bytes)
--    3000 | (1.68 secs, 945082664 bytes)
--    4000 | (3.06 secs, 1672758504 bytes)

-- ---------------------------------------------------------------------
-- § Ordenación rápida (Quicksort)                                    --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Para ordenar una lista xs mediante el algoritmo de
-- ordenación rápida se selecciona el primer elemento x de xs, se divide
-- los restantes en los menores o iguales que x y en los mayores que x,
-- se ordena cada una de las dos partes y se unen los resultados. Por
-- ejemplo, para ordenar la lista [3,1,4,1,5,9,2] el proceso es el
-- siguiente: 
--       or [3,1,4,1,5,9,2]
--     = or [1,1,2] ++ [3] ++ or [4,5,9]  
--     = (or [1] ++ [1] ++ or [2]) ++ [3] ++ (or [] ++ [4] ++ or [5,9])
--     = ((or [] ++ [1] ++ or []) ++ [1] ++ (or [] ++ [2] ++ or [])) 
--       ++ [3] ++ ([] ++ [4] ++ (or [] ++ [5] ++ or [9]))
--     = (([] ++ [1] ++ []) ++ [1] ++ ([] ++ [2] ++ [])) 
--       ++ [3] ++ ([4] ++ ([] ++ [5] ++ (or [] ++ [9] ++ or [])))
--     = ([1] ++ [1] ++ [2] ++ 
--       ++ [3] ++ ([4] ++ ([5] ++ (or [] ++ [9] ++ or [])))
--     = ([1] ++ [1] ++ [2] ++ 
--       ++ [3] ++ ([4] ++ ([5] ++ ([] ++ [9] ++ [])))
--     = ([1] ++ [1] ++ [2] ++ 
--       ++ [3] ++ ([4] ++ ([5] ++ [9]))
--     = [1,1,2,3,4,5,9]
--
-- Definir la función 
--    ordenaRapida :: Ord a => [a] -> [a]
-- tal que (ordenaRapida xs) es la lista obtenida ordenando por
-- selección la lista xs. Por ejemplo,
--    ordenaRapida [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaRapida :: Ord a => [a] -> [a]
ordenaRapida (x:xs) = ordenaRapida [n | n <- xs, n <= x] ++
                      x: ordenaRapida [n | n <- xs, n > x]
ordenaRapida _      = []
     
-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaRapida [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
-- 
-- ¿Cuál es el orden de complejidad de ordenaRapida?
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+------
--    1000 | (0.41 secs, 144970048 bytes)
--    2000 | (1.34 secs, 648669944 bytes)
--    3000 | (3.16 secs, 1434435216 bytes)
--    4000 | (5.88 secs, 2637825336 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Definir, usando un acumulador, la función
--    ordenaRapida2 :: Ord a => [a] -> [a]
-- tal que (ordenaRapida2 xs) es la lista obtenida ordenando xs
-- por el procedimiento de ordenación rápida. Por ejemplo, 
--    ordenaRapida2 [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaRapida2 :: Ord a => [a] -> [a]
ordenaRapida2 [] = []
ordenaRapida2 (x:xs) = divide x xs [] []
  where divide :: Ord a => a -> [a] -> [a] -> [a] -> [a]
        divide n (x:xs) ys zs | x <= n = divide n xs (x:ys) zs
                              | otherwise = divide n xs ys (x:zs)
        divide n  _     ys zs = ordenaRapida2 ys ++
                                n : ordenaRapida2 zs
                                
-- ---------------------------------------------------------------------
-- Ejercicio 2.4. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaRapida2 [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+------
--    1000 | (0.28 secs, 116635696 bytes)
--    2000 | (0.87 secs, 493818120 bytes)
--    3000 | (1.89 secs, 1137327968 bytes)
--    4000 | (3.26 secs, 2029114048 bytes)

-- ---------------------------------------------------------------------
-- § Ordenación por inserción                                         --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Para ordenar una lista xs mediante el algoritmo de
-- ordenación por inserción se selecciona el primer elemento x de xs, se
-- ordena el resto de xs y se inserta x en su lugar. Por ejemplo, para
-- ordenar la lista [3,1,4,1,5,9,2] el proceso es el siguiente: 
--      ordenaPorInsercion [3,1,4,1,5,9,2]
--    = 3 : ordenaPorInsercion [1,4,1,5,9,2]
--    = 3 : 1 : ordenaPorInsercion [4,1,5,9,2]
--    = 3 : 1 : 4 : ordenaPorInsercion [1,5,9,2]
--    = 3 : 1 : 4 : 1 : ordenaPorInsercion [5,9,2]
--    = 3 : 1 : 4 : 1 : 5 : ordenaPorInsercion [9,2]
--    = 3 : 1 : 4 : 1 : 5 : 9 : ordenaPorInsercion [2]
--    = 3 : 1 : 4 : 1 : 5 : 9 : 2 : ordenaPorInsercion []
--    = 3 : 1 : 4 : 1 : 5 : 9 : 2 : []
--    = 3 : 1 : 4 : 1 : 5 : 9 : [2]
--    = 3 : 1 : 4 : 1 : 5 : [2,9]
--    = 3 : 1 : 4 : 1 : [2,5,9]
--    = 3 : 1 : 4 : [1,2,5,9]
--    = 3 : 1 : [1,2,4,5,9]
--    = 3 : [1,1,2,4,5,9]
--    = [1,1,2,3,4,5,9]
--
-- Definir la función 
--    ordenaPorInsercion :: Ord a => [a] -> [a]
-- tal que (ordenaPorInsercion xs) es la lista obtenida ordenando por
-- selección la lista xs. Por ejemplo,
--    ordenaPorInsercion [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaPorInsercion :: Ord a => [a] -> [a]
ordenaPorInsercion [] = []
ordenaPorInsercion (x:xs) = inserta x (ordenaPorInsercion xs)
  where inserta :: Ord a => a -> [a] -> [a]
        inserta x xs = ys ++ x:zs
          where (ys,zs) = divide x xs []
        divide :: Ord a => a -> [a] -> [a] -> ([a],[a])
        divide n [] ys     = (reverse ys,[])
        divide n (x:xs) ys | x < n     = divide n xs (x:ys)
                           | otherwise = (reverse ys,x:xs)

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaPorInsercion [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
--
-- ¿Cuál es la complejidad de ordenaPorInsercion?
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | (0.42 secs, 146571392 bytes)
--    2000 | (1.44 secs, 581844984 bytes)
--    3000 | (3.20 secs, 1297291496 bytes)
--    4000 | (5.55 secs, 2306353936 bytes)


-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Definir, por plegados, la función
--    ordenaPorInsercion2 :: Ord a => [a] -> [a]
-- tal que (ordenaPorInsercion2 xs) es la lista obtenida ordenando xs
-- por el procedimiento de ordenación por inserción. Por ejemplo, 
--    ordenaPorInsercion2 [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaPorInsercion2 :: Ord a => [a] -> [a]
ordenaPorInsercion2 = foldr inserta []
  where inserta :: Ord a => a -> [a] -> [a]
        inserta x xs = ys ++ x:zs
          where (ys,zs) = divide x xs []
        divide :: Ord a => a -> [a] -> [a] -> ([a],[a])
        divide n [] ys     = (reverse ys,[])
        divide n (x:xs) ys | x < n     = divide n xs (x:ys)
                           | otherwise = (reverse ys,x:xs)
                                         
-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaPorInsercion2 [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | (0.41 secs, 143784784 bytes)
--    2000 | (1.46 secs, 582651672 bytes)
--    3000 | (3.18 secs, 1301973744 bytes)
--    4000 | (5.55 secs, 2314743088 bytes)

-- ---------------------------------------------------------------------
-- § Ordenación por mezcla ("Mergesort")                              --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Para ordenar una lista xs mediante el algoritmo de
-- ordenación por mezcla se divide xs por la mitad, se ordena cada una
-- de las partes y se mezclan los resultados. Por ejemplo, para
-- ordenar la lista [3,1,4,1,5,9,2] el proceso es el siguiente: 
--      om [3,1,4,1,5,9,2]
--    = m (om [3,1,4]) (om 1,5,9,2])  
--    = m (m (om [3]) (om [1,4])) (m (om [1,5]) (om [9,2]))
--    = m (m [3] (m (om [1]) (om [4]))) 
--        (m (m (om [1]) (om [5])) (m (om [9]) (om [2])))
--    = m (m [3] (m [1] [4])) 
--        (m (m [1] [5]) (m [9] [2]))
--    = m (m [3] [1,4]) (m [1,5] [2,9])
--    = m [1,3,4] [1,2,5,9]
--    = [1,1,2,3,4,5,9]
-- donde om es ordenaPorMezcla y m es mezcla.
--
-- Definir la función 
--    ordenaPorMezcla :: Ord a => [a] -> [a]
-- tal que (ordenaPorMezcla xs) es la lista obtenida ordenando por
-- selección la lista xs. Por ejemplo,
--    ordenaPorMezcla [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaPorMezcla :: Ord a => [a] -> [a]
ordenaPorMezcla [] = []
ordenaPorMezcla [x] = [x]
ordenaPorMezcla xs = mezcla (ordenaPorMezcla zs) (ordenaPorMezcla ys)
  where (zs,ys) = splitAt (length xs `div` 2) xs
        mezcla :: Ord a => [a] -> [a] -> [a]
        mezcla    []     []  = []
        mezcla    xs     []  = xs
        mezcla    []     ys  = ys
        mezcla (x:xs) (y:ys) | x < y     = x : mezcla xs (y:ys)
                             | otherwise = y : mezcla (x:xs) ys

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaPorMezcla [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
--
-- ¿Cuál es la complejidad de ordenaPorMezcla?
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 0.02
--    2000 | 0.03
--    3000 | 0.05
--    4000 | 0.06

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Otra forma de ordenar una lista xs mediante el
-- algoritmo de ordenación por mezcla consiste en dividir xs en listas
-- unitarias y mezclar los resultados. Por ejemplo, para
-- ordenar la lista [3,1,4,1,5,9,2] el proceso es el siguiente: 
--      om [3,1,4,1,5,9,2]
--    = mp [[3],[1],[4],[1],[5],[9],[2]]
--    = mp [[1,3],[1,4],[5,9],[2]]
--    = mp [[1,1,3,4],[2,5,9]]
--    = [1,1,2,3,4,5,9]
-- donde om es ordenaPorMezcla y mp es mezclaPares.
--
-- Definir la función 
--    ordenaPorMezcla2 :: Ord a => [a] -> [a]
-- tal que (ordenaPorMezcla2 xs) es la lista obtenida ordenando por
-- selección la lista xs. Por ejemplo,
--    ordenaPorMezcla2 [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaPorMezcla2 :: Ord a => [a] -> [a]
ordenaPorMezcla2 xs = foldr1 mezcla [[x] | x <- xs]
  where mezcla :: Ord a => [a] -> [a] -> [a]
        mezcla    []     []  = []
        mezcla    xs     []  = xs
        mezcla    []     ys  = ys
        mezcla (x:xs) (y:ys) | x < y     = x : mezcla xs (y:ys)
                             | otherwise = y : mezcla (x:xs) ys

-- ---------------------------------------------------------------------
-- Ejercicio 4.4. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaPorMezcla2 [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | (0.41 secs, 143784784 bytes)
--    2000 | (1.46 secs, 582651672 bytes)
--    3000 | (3.18 secs, 1301973744 bytes)
--    4000 | (5.55 secs, 2314743088 bytes)

-- ---------------------------------------------------------------------
-- § Ordenación por montículos ("heapsort")                           --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. El procedimiento de ordenación de una lista por
-- montículos consiste en almacenar todos los elementos del vector a
-- ordenar en un montículo (heap), y luego extraer el nodo que queda
-- como nodo raíz del montículo (cima) en sucesivas iteraciones
-- obteniendo el conjunto ordenado.  
-- 
-- Usando la implementación de las colas de prioridad mediante
-- montículos (que se encuentra en la librería I1M.ColaDePrioridad),
-- definir la función 
--    ordenaPorMonticulos :: Ord a => [a] -> [a]
-- tal que (ordenaPorMonticulos xs) es la lista obtenida ordenando xs
-- por el procedimiento de ordenación por montículos. Por ejemplo, 
--    ordenaPorMonticulos [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaPorMonticulos :: Ord a => [a] -> [a]
ordenaPorMonticulos = aLista . foldr CP.inserta CP.vacia
  where aLista :: Ord a => CP.CPrioridad a -> [a]
        aLista c | CP.esVacia c = []
                 | otherwise    = CP.primero c: aLista (CP.resto c)
                      
-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Calcular los tiempos necesarios para calcular 
--     let n = k in length (ordenaPorMonticulos [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
--
-- ¿Cuál es la complejidad de ordenaPorMonticulos?
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | (0.01 secs, 0 bytes)
--    2000 | (0.02 secs, 0 bytes)
--    3000 | (0.03 secs, 12994216 bytes)
--    4000 | (0.04 secs, 13917008 bytes)
-- Coste O(n)
