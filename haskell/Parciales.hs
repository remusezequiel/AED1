{------EJERCICIO -------------------------------------------------------------------------------
Programe la función esMultiploDe :: Integer -> Integer -> Bool 

que devuelve verdadero si el segundo parámetro es múltiplo del primero. Por ejemplo:

         esMultiploDe 3 33 devuelve True
         esMultiploDe 17 23 devuelve False
      Aclaración: para este ejercicio no está permitido utilizar div, mod ,/ ,* ni equivalentes..
-------------------------------------------------------------------------------------------------}

{- resto 
 - 
 - Calcula el resto de dividir (entera) x por y usando solo la resta
 - siendo estos enteros
 -}
resto :: Integer -> Integer -> Integer
resto x y | x == y = 0
                | x >= y = resto (x-y) y
                | x <= y = x
{-
 - Un numero q es multiplo de un numero p, cuando
 - p entra una determinada cantidad de veces k dentro
 - de q
-}
esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe p q | resto q p == 0 = True 
                 | otherwise = False

{-------EJERCICIO -------------------------------------------------------------------------------
 - Programe la función intercalar :: [a] -> [a] -> [a] 
 - 
 - que recibe dos listas de igual longitud y devuelve una lista 
 - del doble de esa longitud, con los elementos de ambas listas intercalados. Por ejemplo:
 -
 -    intercalar [107,33,45] [28,42,37] devuelve [107,28,33,42,45,37]
------EJERCICIO --------------------------------------------------------------------------------}

long :: [a] -> Integer
long [] = 0
long (x:xs) = 1 + long (xs)

intercalarAux :: [a] -> [a] -> [a]
intercalarAux (x:xs) (y:ys)
    | long(x:xs) == long(y:ys) = x:[y]  
    | otherwise = []

intercalar :: [a] -> [a] -> [a]
intercalar [] [] = []
intercalar (x:xs) (y:ys) 
    | long (y:ys) == long (x:xs) = (intercalarAux (x:xs) (y:ys))++(intercalar xs ys)   
    |otherwise = []
{-------EJERCICIO -------------------------------------------------------------------------------
 - Programe la función quitarTodosLos :: Integer -> [Integer] -> [Integer] 
 -
 - de modo tal que quitarTodosLos x xs devuelva la lista xs pero sin ninguna 
 - aparición del elemento x. Por ejemplo:
 -
 -    quitarTodosLos 8 [8,5,3,9,8,2,8,7] devuelve [5,3,9,2,7]
------EJERCICIO --------------------------------------------------------------------------------}
quitarUno :: Integer -> [Integer] -> [Integer]
quitarUno p (x:xs) | p /= x = x : (quitarUno p xs)
                   | p == x = xs 

quitarTodosLos :: Integer -> [Integer] -> [Integer]
quitarTodosLos p [] = []
quitarTodosLos p (x:xs) | p /= x = x : quitarTodosLos p xs 
                        | otherwise =  quitarTodosLos p (quitarUno p (x:xs))

{-------EJERCICIO --------------------------------------------------------------------------------
 - Programe la función comprimir :: [Integer] -> [Integer] -> [(Integer,Integer)] 
 - 
 - que devuelva una lista que contenga una tupla (elemento, cantVeces) por 
 - cada tira de elementos iguales adyacentes. Por ejemplo:
 - 
 - comprimir [7,7,4,4,4,4,4,3,3,3] devuelve [(7,2),(4,5),(3,3)]
 - Sugerencia: empiece reemplazando cada elemento e por una tupla (e,1)
 ------EJERCICIO ---------------------------------------------------------------------------------}

contarRepetido :: Integer -> [Integer] -> Integer
contarRepetido p [] = 0
contarRepetido p (x:xs) | p == x = 1 + contarRepetido p xs
                        | otherwise = 0 
 
 
comprimir :: [Integer] -> [(Integer,Integer)]
comprimir [] = []
comprimir (x:xs) | x /= (head xs) = [(x, 1)] ++ comprimir xs
                 | x == (head xs) = [(x,contarRepetido x (x:xs))] ++ comprimir (quitarTodosLos x (x:xs)) 


{-------EJERCICIO --------------------------------------------------------------------------------
 - Programe la función esFibonacci :: Integer -> Bool 
 - 
 - tal que si esFibonacci n devuelve True si n  pertenece a la sucesion de fibonacci
 - 
 - Ejemplo : esFibonacci 5 -> True
 -           esFibonacci 7 -> False 
 - Sugerencia: sucesión de Fibonacci : fib (n+2) = fib n + fib (n+1)
 -             ademas, fib 1 = fib 2 = 1
 ------EJERCICIO ---------------------------------------------------------------------------------}

fib :: Integer -> Integer
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

fibAux :: Integer -> Integer -> Bool
fibAux n s | fib s == n = True
           | fib s > n = False
           | fib s < n = fibAux n (s+1)

esFibonacci :: Integer -> Bool
esFibonacci n = fibAux n 1



















 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 