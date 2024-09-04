module Clase11
where

import Clase10

--(coeficiente, grado)
type Monomio = (Float, Integer)
--(a,b) donde a/b
type Racional = (Integer, Integer) 

--Aca tenes que fijarte siempre que sean de igual grado
resta :: Polinomio -> Polinomio -> Polinomio
resta p q = suma p (productoPorEscalar (-1) q)

primerCociente :: Polinomio -> Polinomio -> Monomio
primerCociente p q | grado p >= grado q = ((head p) / (head q), grado p - grado q)

sumaMonomioPolinomio :: Monomio -> Polinomio -> Polinomio
sumaMonomioPolinomio m p = suma (productoPorMonomio m [1]) p

primerResto :: Polinomio -> Polinomio -> Polinomio
primerResto p q = resta p h
                  where h = productoPorMonomio (primerCociente p q) q

divisionPol :: Polinomio -> Polinomio -> (Polinomio,Polinomio)
divisionPol p [] = undefined 
divisionPol [] q = ([],[])
divisionPol p q | grado p < grado q = ([],p)
             | grado p >= grado q = (sumaMonomioPolinomio (primerCociente p q) cr, rr) 
               where (cr, rr) = divisionPol (primerResto p q) q 

mcdNM :: Polinomio -> Polinomio -> (Polinomio,Polinomio)
mcdNM [] [] = undefined
mcdNM [] q = q
mcdNM q [] = q
mcdNM p q = mcdNM q (snd(divisionPol p q))

hacerMonico :: Polinomio -> Polinomio
hacerMonico [] = undefined
hacerMonico p = productoPorEscalar (1 / head p) p

mcdP :: Polinomio -> Polinomio -> Polinomio
mcdP p q = hacerMonico (mcdNM p q)

multiplicidad :: Float -> Polinomio -> Integer
multiplicidad f p | evaluar p f /= 0 = 0
                  | otherwise = 1 + multiplicidad f (fst (divisionPol p [1,-f]))

raicesMultiples :: Polinomio -> Bool 
raicesMultiples p = grado (mcdP p (derivada p)) > 0

{-
esRaizRacional :: Polinomio -> Racional -> Bool
esRaizRacional p (a,b) = evaluar p (realToFrac a / realToFrac b) == 0 

raicesEnConjunto :: Polinomio -> [Racional] -> [Racional] 
raicesEnConjunto _ [] = []
raicesEnConjunto p (x:xs) | esRaizRacional p x = x:(raicesEnConjunto p xs)
                          | otherwise = raicesEnConjunto p xs 

divisores :: Integer -> [Integer]

soloPositivos :: [Integer] -> [Integer]

--Esta es justamente el producto carteciano
armarRacionales :: [Integer] -> [Integer] -> [Racional] 


candidatosRaices :: Polinomio -> [Racional]
candidatosRaices p = (armarRacionales (divisores (last p))) * (soloPositivos (divisores (head p)))

raicesRacionales :: Polinomio -> [Racional]
raicesRacionales p = raicesEnConjunto p (candidatosRaices p)

-}

