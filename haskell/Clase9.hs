module Clase9
where

{- ALGORITMO DE EUCLIDES

     Calcula el maximo comun divisor de 
     dos numeros "a" y "b" enteros.

Se basa en que si "a", "b" enteros y k entero: 

(a:b)=(a+kb : b)

q=cociente y r=resto => a = qb+r => r=a-qbr 


-}


------------------------------------------------------------
resto :: Integer -> Integer -> Integer
resto a b | a == b = 0
          | a < b = a
          | a > b = resto (a - b) b

mcd :: Integer -> Integer -> Integer
mcd a b | a > 0 && b >= 0 = a
        | a < b = mcd b a
        | otherwise = mcd (resto a b) b 
------------------------------------------------------------

------------------------------------------------------------
alter_mcd :: Integer -> Integer -> Integer
alter_mcd a 0 = a 
alter_mcd a b = mcd b (mod a b)
------------------------------------------------------------


------------------------------------------------------------
listaDivisores :: Integer -> Integer -> [Integer]
listaDivisores n y | y == n = [n]
                   | mod n y == 0 = y : listaDivisores n (y + 1)
                   | otherwise = listaDivisores n (y + 1)
------------------------------------------------------------

------------------------------------------------------------
mcd_Hasta :: Integer -> Integer -> Integer -> Integer
mcd_Hasta a b 1 = 1
mcd_Hasta a b k | (mod a k == 0) && (mod b k == 0) = k 
                | otherwise = mcd_Hasta a b (k - 1)


mcd_Dos :: Integer -> Integer -> Integer      
mcd_Dos a b = mcd_Hasta a b a
------------------------------------------------------------

--Extended mcd

emcd :: Integer -> Integer -> (Integer, Integer, Integer)
emcd a 0 = (a, 1, 0)
emcd a b = (g, t, s - t * q)
           where (g, s, t) = emcd b (mod a b)
                 q = div a b


tieneSolucion :: Integer -> Integer -> Integer -> Bool 
tieneSolucion a b m | (mod b (mcd a m)) == 0 = True
                    | otherwise = False


solucionParticular :: Integer -> Integer -> Integer -> Integer
solucionParticular a b m | tieneSolucion a m b = x * (div b g)
                           where (g, x, k) = emcd a m  
                         
--solucionGeneral :: Integer -> Integer -> Integer -> (Integer, Integer)
--solucionGeneral a b m = (solucionParticular a b m , b * (div) )






































