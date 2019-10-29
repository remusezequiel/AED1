module Simulacro
where
 
-- Ejercicio 1

menorLex :: (Float, Float, Float) -> (Float, Float, Float) -> Bool
menorLex (x1, y1, z1) (x2, y2, z2) | x1 <= x2 || y1 <= y2 ||  z1 <= z2 = True
                                       | otherwise = False

-- Ejercicio 2

fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 1
                  | n == 1 = 1
                  | n > 1 = fibonacci (n - 2) + fibonacci (n -1)

sumaFibonacci :: Integer -> Integer
sumaFibonacci n | n == 0 = 1
             | otherwise = sumaFibonacci (n -1) + fibonacci n  

-- Ejercicio 3
sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta 1 _ = 1
sumaDivisoresHasta k n | (mod n k) == 0 = k + sumaDivisoresHasta (k - 1) n
                          | otherwise = sumaDivisoresHasta (k - 1) n

esDefectivo :: Integer -> Bool
esDefectivo n  = (sumaDivisoresHasta (n - 1) n) < n

--Ejercicio 4

maximaDistancia :: [Integer] -> Integer
maximaDistancia (x : y : []) = abs(y - x)
maximaDistancia (x : y : xs) = max (abs (y - x)) (maximaDistancia(y : xs))








