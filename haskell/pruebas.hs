type Circulo = [Integer]
-------- PERMUTACIONES ----------------------------------------------
longitud :: [a] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

factorial :: Integer -> Integer
factorial n | n == 0 = 1
           | n > 0 = n  * factorial (n - 1)

cantidadDePermutaciones :: Integer -> Integer
cantidadDePermutaciones n = factorial n

union :: Eq a => [a] -> [a] -> [a]
union [] ys = ys
union (x:xs) ys | elem x ys = union xs ys
                | otherwise = union xs (x:ys)

listaDeN :: Integer -> [Integer]
listaDeN 1 = union [1] []                              
listaDeN 2 = union [2] [1]  
listaDeN n | (longitud (union [n-1] [n]) < n) = union [n] [] ++ listaDeN (n - 1)
             
listaInversa :: [Integer] -> [Integer]
listaInversa [] = []
listaInversa (x:xs) = listaInversa xs ++ [x] 

crearLista :: Integer -> [Integer]
crearLista n = listaInversa (listaDeN n)

permutaUno :: [Integer] -> [Integer]
permutaUno [1,2] = [2,1]
permutaUno (x:xs) = [x] ++ listaInversa (xs)

listasIguales :: [Integer] -> [Integer] -> Bool
listasIguales [] [] = True
listasIguales (x:xs) (y:ys) | x == y = listasIguales xs ys
                            | otherwise = False

add_permutaUno :: [Integer] -> [[Integer]]
add_permutaUno (x:xs) = [(x:xs)] ++ [permutaUno (x:xs)]

add :: [Integer] -> [[Integer]]
add [1,2] = add_permutaUno [1,2]
add (x:xs) | sonCirculosIguales (x:xs) (permutaUno (x:xs)) = False == add_permutaUno (x:xs) ++ add (listaInversa (x:xs))

--permutaciones :: Integer -> [[Integer]]
--permutaciones n | cantidadDePermutaciones n /= longitud (crearLista n) = 

-------------------------------------------------------------------------------------
{-
intercala :: a -> [a] -> [[a]]
intercala x [] = [[x]]
intercala x (y:ys) = (x:y:ys) : [y:zs | zs <- intercala x ys]

paraPermutaciones :: [a] -> [[a]]
paraPermutaciones []     = [[]]
paraPermutaciones (x:xs) = concat [intercala x ys | ys <- paraPermutaciones xs]
 
permutaciones :: Integer -> [[Integer]]
permutaciones n = paraPermutaciones (crearLista n) 
 
permutacionesN :: Integer -> [[Integer]]
permutacionesN n = paraPermutaciones [1..n]
-}
------------------------------------------------------------------------------- 
 
{- 
combinations :: Int -> [a] -> [[a]] 
combinations k xs = combinations' (length xs) k xs 
    where combinations' n k' l@(y:ys) | k' == 0 = [[]] | k' >= n = [l] | null l = [] | otherwise = map (y :) (combinations' (n - 1) (k' - 1) ys) ++ combinations' (n - 1) k' ys 
-}

