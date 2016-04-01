-- I1M 2015-16: Relación 29 (29 de marzo de 2016)
-- Números de Lychrel.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================
-- EDIT
-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- Un número de Lychrel es un número natural para el que nunca se
-- obtiene un capicúa mediante el proceso de invertir las cifras y sumar
-- los dos números. Por ejemplo, los siguientes números no son números
-- de Lychrel: 
--    * 56, ya que en un paso se obtiene un capicúa: 56+65=121.
--    * 57, ya que en dos pasos se obtiene un capicúa: 57+75=132,
--      132+231=363
--    * 59, ya que en dos pasos se obtiene un capicúa: 59+95=154,
--      154+451=605, 605+506=1111
--    * 89, ya que en 24 pasos se obtiene un capicúa.
-- En esta relación vamos a buscar el primer número de Lychrel.

-- ---------------------------------------------------------------------
-- Librerías auxiliares                                               --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    esCapicua :: Integer -> Bool
-- tal que (esCapicua x) se verifica si x es capicúa. Por ejemplo,
--    esCapicua 252  ==  True
--    esCapicua 253  ==  False
-- ---------------------------------------------------------------------

esCapicua :: Integer -> Bool
esCapicua x = sx == reverse sx
  where sx = show x

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    inverso :: Integer -> Integer
-- tal que (inverso x) es el número obtenido escribiendo las cifras de x
-- en orden inverso. Por ejemplo,
--    inverso 253  ==  352
-- ---------------------------------------------------------------------

inverso :: Integer -> Integer
inverso = read . reverse . show

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    siguiente :: Integer -> Integer
-- tal que (siguiente x) es el número obtenido sumándole a x su
-- inverso. Por ejemplo,
--    siguiente 253  ==  605
-- ---------------------------------------------------------------------

siguiente :: Integer -> Integer
siguiente x = x + inverso x

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    busquedaDeCapicua :: Integer -> [Integer]
-- tal que (busquedaDeCapicua x) es la lista de los números tal que el
-- primero es x, el segundo es (siguiente de x) y así sucesivamente
-- hasta que se alcanza un capicúa. Por ejemplo,
--    busquedaDeCapicua 253  ==  [253,605,1111]
-- ---------------------------------------------------------------------

busquedaDeCapicua :: Integer -> [Integer]
busquedaDeCapicua = takeWhileCapicua' . iterate siguiente
  where takeWhileCapicua' :: [Integer] -> [Integer]
        takeWhileCapicua' (x:xs) | esCapicua x = [x]
                                 | otherwise   = x: takeWhileCapicua' xs

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    capicuaFinal :: Integer -> Integer
-- tal que (capicuaFinal x) es la capicúa con la que termina la búsqueda
-- de capicúa a partir de x. Por ejemplo,
--    capicuaFinal 253  ==  1111
-- ---------------------------------------------------------------------

capicuaFinal :: Integer -> Integer
capicuaFinal = head . dropWhile (not.esCapicua) . iterate siguiente

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    orden :: Integer -> Integer
-- tal que (orden x) es el número de veces que se repite el proceso de
-- calcular el inverso a partir de x hasta alcanzar un número
-- capicúa. Por ejemplo,
--    orden 253  ==  2
-- ---------------------------------------------------------------------

orden :: Integer -> Integer
orden x | esCapicua x = 0
        | otherwise   = 1 + orden (siguiente x)

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    ordenMayor :: Integer -> Integer -> Bool
-- tal que (ordenMayor x n) se verifica si el orden de x es mayor o
-- igual que n. Dar la definición sin necesidad de evaluar el orden de
-- x. Por ejemplo,
--    ghci> ordenMayor 1186060307891929990 2
--    True
--    ghci> orden 1186060307891929990
--    261
-- ---------------------------------------------------------------------

ordenMayor :: Integer -> Integer -> Bool
ordenMayor x 0 = True
ordenMayor x n = not (esCapicua x) && ordenMayor (siguiente x) (n-1)

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    ordenEntre :: Integer -> Integer -> [Integer]
-- tal que (ordenEntre m n) es la lista de los elementos cuyo orden es
-- mayor o igual que m y menor que n. Por ejemplo,
--    take 5 (ordenEntre 10 11)  ==  [829,928,9059,9149,9239]
-- ---------------------------------------------------------------------

ordenEntre :: Integer -> Integer -> [Integer]
ordenEntre m n = filter (\x -> ordenMayor x m && ordenMenor x n) [1..]
  where ordenMenor :: Integer -> Integer -> Bool
        ordenMenor _ 0 = False
        ordenMenor x n = esCapicua x || ordenMenor (siguiente x) (n-1)

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    menorDeOrdenMayor :: Integer -> Integer
-- tal que (menorDeOrdenMayor n) es el menor elemento cuyo orden es
-- mayor que n. Por ejemplo,
--    menorDeOrdenMayor 2   ==  19
--    menorDeOrdenMayor 20  ==  89
-- ---------------------------------------------------------------------

menorDeOrdenMayor :: Integer -> Integer
menorDeOrdenMayor n = head [x | x <- [1..], ordenMayor x n]

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función 
--    menoresdDeOrdenMayor :: Integer -> [(Integer,Integer)]
-- tal que (menoresdDeOrdenMayor m) es la lista de los pares (n,x) tales
-- que n es un número entre 1 y m y x es el menor elemento de orden
-- mayor que n. Por ejemplo,
--    menoresdDeOrdenMayor 5  ==  [(1,10),(2,19),(3,59),(4,69),(5,79)]
-- ---------------------------------------------------------------------

menoresDeOrdenMayor :: Integer -> [(Integer,Integer)]
menoresDeOrdenMayor m = busca [1..m] [1..]
  where busca :: [Integer]-> [Integer] -> [(Integer,Integer)]
        busca []     _  = []
        busca (n:ns) xs =  (n, head zs) : busca ns zs
          where zs = dropWhile (\x -> not $ ordenMayor x n) xs

-- ---------------------------------------------------------------------
-- Ejercicio 11. A la vista de los resultados de (menoresdDeOrdenMayor 5)
-- conjeturar sobre la última cifra de menorDeOrdenMayor.
-- ---------------------------------------------------------------------
-- ¿0 o 9?

-- ---------------------------------------------------------------------
-- Ejercicio 12. Decidir con QuickCheck la conjetura.
-- ---------------------------------------------------------------------

-- La conjetura es
prop_menorDeOrdenMayor :: (Positive Integer) -> Bool
prop_menorDeOrdenMayor (Positive n) = 
  all (\x -> x == 0 || x == 9) $ map (`mod` 10) $ map snd $
      menoresDeOrdenMayor n

-- La comprobación es
-- *** Failed! Falsifiable (after 30 tests and 1 shrink): 
-- Positive {getPositive = 25}

-- ---------------------------------------------------------------------
-- Ejercicio 13. Calcular (menoresdDeOrdenMayor 50)
-- ---------------------------------------------------------------------

-- Solución: El cálculo es
{- [(1,10),(2,19),(3,59),(4,69),(5,79),(6,79),(7,89),(8,89),(9,89),
   (10,89),(11,89),(12,89),(13,89),(14,89),(15,89),(16,89),(17,89),
   (18,89),(19,89),(20,89),(21,89),(22,89),(23,89),(24,89),(25,196),
   (26,196),(27,196),(28,196),(29,196),(30,196),(31,196),(32,196),
   (33,196),(34,196),(35,196),(36,196),(37,196),(38,196),(39,196),
   (40,196),(41,196),(42,196),(43,196),(44,196),(45,196),(46,196),
   (47,196),(48,196),(49,196),(50,196)] -}

-- ---------------------------------------------------------------------
-- Ejercicio 14. A la vista de (menoresdDeOrdenMayor 50), conjeturar el
-- orden de 196. 
-- ---------------------------------------------------------------------
-- ¿Primer número de Lychrel?
-- ---------------------------------------------------------------------
-- Ejercicio 15. Comprobar con QuickCheck la conjetura sobre el orden de
-- 196. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_ordenDe196 (Positive n) = ordenMayor 196 n

-- La comprobación es
-- λ> quickCheck prop_ordenDe196
-- +++ OK, passed 100 tests.
 
