module Clase5
where

import Clase4

--MAS RECURSIVIDAD

{-Fijate que n_esImpar esta usando los parametros sin tener que definirselos -}
sumaLosPrimerosNImpares :: Integer -> Integer
sumaLosPrimerosNImpares n | n == 1 = 1
                               | n > 1 = n_esImpar + sumaLosPrimerosNImpares (n - 1)
                                  where n_esImpar | mod n 2 /= 0 = n 
                                                       | otherwise = 0

eAprox :: Integer -> Float
eAprox n | n == 0 = 1 
          | n > 0 = eAprox (n - 1) + (fromInteger 1) / (fromInteger (factorial n)) 

--Numero e             
e :: Float
e = eAprox 100           

--Implenentar una funcion parteEntera :: Float -> Integer que calcule la parte entera de un numero real positivo
parteEntera :: Float -> Integer
parteEntera x | x == 0 || (x < 1 && x > 0) = 0 
               | x == 1 || (x < 2 && x > 1) = 1
               | otherwise = 1 + parteEntera (x - 1)

--Modificar la funcion parteEntera para que tambien funciones con numeros negativos
parteEnteraNeg x | x < 1 && x >=0 = 0 -- (0 <= x < 1)
                   | x >= (-1) && x < 0 = (-1) --(0 > x >= -1)
                   | x >= 1 = 1 + parteEnteraNeg (x - 1) --(x >= 1)
                   | x <= (-1) = (-1) + parteEnteraNeg (x + 1)--(x < -1)

--Dos codigos para la funcion division la cual utiliza el algoritmo de division
division :: Integer -> Integer -> (Integer, Integer)
division a d | a < d = (0, a)
              |otherwise = (fst qr' + 1, snd qr')
               where qr' = division (a - d) d


















