-- I1M 2015-16: Relación 33 (22 de abril de 2016)
-- Operaciones con el TAD de polinomios.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación es ampliar el conjunto de operaciones
-- sobre polinomios definidas utilizando las implementaciones del TAD de
-- polinomio estudiadas en el tema 21 
--    http://www.cs.us.es/~jalonso/cursos/i1m-15/temas/tema-21.html
-- 
-- Además, en algunos ejemplos de usan polinomios con coeficientes
-- racionales. En Haskell, el número racional x/y se representa por
-- x%y. El TAD de los números racionales está definido en el módulo
-- Data.Ratio.   
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
-- implementación del TAD de polinomios: 
-- + PolRepTDA      que está en http://bit.ly/1WJnS93
-- + PolRepDispersa que está en http://bit.ly/1WJnUO8
-- + PolRepDensa    que está en http://bit.ly/1WJnV4E 
-- + PolOperaciones que está en http://bit.ly/1WJnTd7

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.Ratio

-- Hay que elegir una librería 
import I1M.PolOperaciones 
-- import PolOperaciones 

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    creaPolDispersa :: (Num a, Eq a) => [a] -> Polinomio a
-- tal que (creaPolDispersa xs) es el polinomio cuya representación
-- dispersa es xs. Por ejemplo,
--    creaPolDispersa [7,0,0,4,0,3]  ==  7*x^5 + 4*x^2 + 3
-- ---------------------------------------------------------------------

creaPolDispersa :: (Num a, Eq a) => [a] -> Polinomio a
creaPolDispersa xs = creaPolDensa (zip ys xs)
  where ys = [n-1,n-2..0]
        n  = length xs

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    creaPolDensa :: (Num a, Eq a) => [(Int,a)] -> Polinomio a
-- tal que (creaPolDensa xs) es el polinomio cuya representación
-- densa es xs. Por ejemplo,
--    creaPolDensa [(5,7),(4,2),(3,0)]  ==  7*x^5 + 2*x^4
-- ---------------------------------------------------------------------

creaPolDensa :: (Num a, Eq a) => [(Int,a)] -> Polinomio a
creaPolDensa = foldr (uncurry consPol) polCero

-- ---------------------------------------------------------------------
-- Nota. En el resto de la sucesión se usará en los ejemplos los
-- los polinomios que se definen a continuación.
-- ---------------------------------------------------------------------

pol1, pol2, pol3 :: (Num a, Eq a) => Polinomio a
pol1 = creaPolDensa [(5,1),(2,5),(1,4)]
pol2 = creaPolDispersa [2,3]
pol3 = creaPolDensa [(7,2),(4,5),(2,5)]

pol4, pol5, pol6 :: Polinomio Rational 
pol4 = creaPolDensa [(4,3),(2,5),(0,3)]
pol5 = creaPolDensa [(2,6),(1,2)]
pol6 = creaPolDensa [(2,8),(1,14),(0,3)]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    densa :: (Num a, Eq a) => Polinomio a -> [(Int,a)]
-- tal que (densa p) es la representación densa del polinomio p. Por
-- ejemplo, 
--    pol1        ==  x^5 + 5*x^2 + 4*x
--    densa pol1  ==  [(5,1),(2,5),(1,4)]
-- ---------------------------------------------------------------------

densa :: (Num a, Eq a) => Polinomio a -> [(Int,a)]
densa p | esPolCero p  = []
        | otherwise    = (grado p, coefLider p) : densa (restoPol p)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    densaAdispersa :: (Num a, Eq a) => [(Int,a)] -> [a]
-- tal que (densaAdispersa ps) es la representación dispersa del
-- polinomio cuya representación densa es ps. Por ejemplo,
--    densaAdispersa [(5,1),(2,5),(1,4)]  ==  [1,0,0,5,4,0]
-- ---------------------------------------------------------------------

densaAdispersa :: (Num a, Eq a) => [(Int,a)] -> [a]
densaAdispersa ((a,b):ys@((c,d):xs)) = b: replicate n 0 ++ 
                                          densaAdispersa ys
  where n = a - c - 1
densaAdispersa [(a,b)] = b: replicate a 0
densaAdispersa _       = [0]

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    dispersa :: (Num a, Eq a) => Polinomio a -> [a]
-- tal que (dispersa p) es la representación dispersa del polinomio
-- p. Por ejemplo,
--    pol1           ==  x^5 + 5*x^2 + 4*x
--    dispersa pol1  ==  [1,0,0,5,4,0]
-- ---------------------------------------------------------------------

dispersa :: (Num a, Eq a) => Polinomio a -> [a]
dispersa = densaAdispersa . densa

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    coeficiente :: (Num a, Eq a) => Int -> Polinomio a -> a
-- tal que (coeficiente k p) es el coeficiente del término de grado k
-- del polinomio p. Por ejemplo,
--    pol1                ==  x^5 + 5*x^2 + 4*x
--    coeficiente 2 pol1  ==  5
--    coeficiente 3 pol1  ==  0
-- ---------------------------------------------------------------------

coeficiente :: (Num a, Eq a) => Int -> Polinomio a -> a
coeficiente k p | k > n     = 0
                | k < n     = coeficiente k (restoPol p)
                | otherwise = coefLider p
  where n = grado p

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    coeficientes :: (Num a, Eq a) => Polinomio a -> [a]
-- tal que (coeficientes p) es la lista de los coeficientes del
-- polinomio p. Por ejemplo,
--    pol1               ==  x^5 + 5*x^2 + 4*x
--    coeficientes pol1  ==  [1,0,0,5,4,0]
-- ---------------------------------------------------------------------

coeficientes :: (Num a, Eq a) => Polinomio a -> [a]
coeficientes = dispersa

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    potencia :: (Num a, Eq a) => Polinomio a -> Int -> Polinomio a
-- tal que (potencia p n) es la potencia n-ésima del polinomio p. Por
-- ejemplo, 
--    pol2             ==  2*x + 3
--    potencia pol2 2  ==  4*x^2 + 12*x + 9
--    potencia pol2 3  ==  8*x^3 + 36*x^2 + 54*x + 27
-- ---------------------------------------------------------------------

potencia :: (Num a, Eq a) => Polinomio a -> Int -> Polinomio a
potencia p n = foldr1 multPol (replicate n p)

-- ---------------------------------------------------------------------
-- Ejercicio 9. Mejorar la definición de potencia definiendo la función
--    potenciaM :: (Num a, Eq a) => Polinomio a -> Int -> Polinomio a
-- tal que (potenciaM p n) es la potencia n-ésima del polinomio p,
-- utilizando las siguientes propiedades:
--    * Si n es par,   entonces x^n = (x^2)^(n/2)
--    * Si n es impar, entonces x^n = x * (x^2)^((n-1)/2)
-- Por ejemplo, 
--    pol2              ==  2*x + 3
--    potenciaM pol2 2  ==  4*x^2 + 12*x + 9
--    potenciaM pol2 3  ==  8*x^3 + 36*x^2 + 54*x + 27
-- ---------------------------------------------------------------------

potenciaM :: (Num a, Eq a) => Polinomio a -> Int -> Polinomio a
potenciaM p 1 = p
potenciaM p n | even n = potenciaM p2 n2
              | otherwise = p `multPol` potenciaM p2 n2
  where p2 = p `multPol` p
        n2 = n `div` 2                         

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    integral :: (Fractional a, Eq a) => Polinomio a -> Polinomio a
-- tal que (integral p) es la integral del polinomio p cuyos coefientes
-- son números racionales. Por ejemplo,
--    ghci> pol3
--    2*x^7 + 5*x^4 + 5*x^2
--    ghci> integral pol3
--    0.25*x^8 + x^5 + 1.6666666666666667*x^3
--    ghci> integral pol3 :: Polinomio Rational
--    1 % 4*x^8 + x^5 + 5 % 3*x^3
-- ---------------------------------------------------------------------

integral :: (Fractional a, Eq a) => Polinomio a -> Polinomio a
integral = creaPolDensa . 
           map (\(a,b) -> (a+1,b/fromIntegral(a+1))) . densa

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--    integralDef :: (Fractional t, Eq t) => Polinomio t -> t -> t -> t          
-- tal que (integralDef p a b) es la integral definida del polinomio p
-- cuyos coefientes son números racionales. Por ejemplo,
--    ghci> integralDef pol3 0 1
--    2.916666666666667
--    ghci> integralDef pol3 0 1 :: Rational
--    35 % 12
-- ---------------------------------------------------------------------

integralDef :: (Fractional t, Eq t) => Polinomio t -> t -> t -> t    
integralDef p a b = valor primitiva b - valor primitiva a
  where primitiva = integral p


-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    multEscalar :: (Num a, Eq a) => a -> Polinomio a -> Polinomio a
-- tal que (multEscalar c p) es el polinomio obtenido multiplicando el
-- número c por el polinomio p. Por ejemplo, 
--    pol2                    ==  2*x + 3
--    multEscalar 4 pol2      ==  8*x + 12
--    multEscalar (1%4) pol2  ==  1 % 2*x + 3 % 4
-- ---------------------------------------------------------------------

multEscalar :: (Num a, Eq a) => a -> Polinomio a -> Polinomio a
multEscalar c p | esPolCero p = polCero
                | otherwise   = consPol n (c*a) $ 
                                multEscalar c (restoPol p)
  where a = coefLider p
        n = grado p

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--    cociente:: (Fractional a, Eq a) => 
--               Polinomio a -> Polinomio a -> Polinomio a
-- tal que (cociente p q) es el cociente de la división de p entre
-- q. Por ejemplo, 
--    pol4  ==  3 % 1*x^4 + 5 % 1*x^2 + 3 % 1
--    pol5  ==  6 % 1*x^2 + 2 % 1*x
--    cociente pol4 pol5  ==  1 % 2*x^2 + (-1) % 6*x + 8 % 9
-- ---------------------------------------------------------------------

cociente:: (Fractional a, Eq a) => Polinomio a ->
           Polinomio a -> Polinomio a
cociente p q | n == 0    = multEscalar (1/x) p
             | m < n     = polCero
             | otherwise = consPol s z (cociente r q)
    where x = coefLider p
          m = grado p
          y = coefLider q
          n = grado q
          s = m-n
          z = x/y
          r = restaPol p (multPorTerm (creaTermino s z) q)

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--    resto :: (Fractional a, Eq a) => 
--             Polinomio a -> Polinomio a -> Polinomio a
-- tal que (resto p q) es el resto de la división de p entre q. Por
-- ejemplo,  
--    pol4  ==  3 % 1*x^4 + 5 % 1*x^2 + 3 % 1
--    pol5  ==  6 % 1*x^2 + 2 % 1*x
--    resto pol4 pol5  ==  (-16) % 9*x + 3 % 1
-- ---------------------------------------------------------------------

resto :: (Fractional a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
resto p q = restaPol p (multPol q (cociente p q))

-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir la función
--    divisiblePol :: (Fractional a, Eq a) => 
--                    Polinomio a -> Polinomio a -> Bool
-- tal que (divisiblePol p q) se verifica si el polinomio p es divisible
-- por el polinomio q. Por ejemplo,
--    pol6  ==  8 % 1*x^2 + 14 % 1*x + 3 % 1
--    pol2  ==  2*x + 3
--    pol5  ==  6 % 1*x^2 + 2 % 1*x
--    divisiblePol pol6 pol2  ==  True
--    divisiblePol pol6 pol5  ==  False
-- ---------------------------------------------------------------------

divisiblePol :: (Fractional a, Eq a) => Polinomio a -> Polinomio a -> Bool
divisiblePol p q = resto p q == polCero

-- ---------------------------------------------------------------------
-- Ejercicio 16. El método de Horner para calcular el valor de un
-- polinomio se basa en representarlo de una forma forma alternativa. Por
-- ejemplo, para calcular el valor de 
--    a*x^5 + b*x^4 + c*x^3 + d*x^2 + e*x + f
-- se representa como
--   ((((a * x + b) * x + c) * x + d) * x + e) * x + f
-- y se evalúa de dentro hacia afuera. 
-- 
-- Definir la función
--    horner:: (Num a, Eq a) => Polinomio a -> a -> a
-- tal que (horner p x) es el valor del polinomio p al sustituir su
-- variable por el número x. Por ejemplo, 
--    horner pol1 0     ==  0
--    horner pol1 1     ==  10
--    horner pol1 1.5   ==  24.84375
--    horner pol1 (3%2) ==  795 % 32
-- ---------------------------------------------------------------------

horner :: (Num a, Eq a) => Polinomio a -> a -> a
horner p x = foldl1 (\a b -> a*x+b) (dispersa p)


