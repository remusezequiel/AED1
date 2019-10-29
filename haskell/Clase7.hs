module Clase7
where


{-    LISTAS 

   Algunas operaciones: 

head :: [a] -> a
tail :: [a] -> [a]
(:) :: a -> [a] -> [a]
(++) :: [a] -> [a] -> [a]
length :: [a] -> Integer

-}


{- Funcion sumatoria: 

  Es un sumador de elementos dentro de una lista
  toma la cabeza y se lo suma de forma recursiva a la
  cabeza de la cola llamandose a si misma
-}
sumatoria :: [Integer] -> Integer
sumatoria lis | length lis == 0 = 0
              | otherwise = (head lis) + sumatoria (tail lis)

{- Funcion pertenece: 
  
  Se fija si un elemento "K" pertenece o no a la lista 
  siendo tanto el elemento como la lista un Integer
  Esto lo hace de forma recursiva, primero evalua si en 
  la cabeza de la lista esta el elemento k, sino se llama
  a si misma pasandose la cola de la lista y el elemento k
  para volver a evaluar sobre la cabeza de la cola. 

-}
pertenece :: Integer -> [Integer] -> Bool
pertenece k lis | length lis == 0 = False
                | k == head lis = True
                | otherwise = pertenece k (tail lis)
------------------------------------------------------------------
--  Definir la función listar :: a-> a -> a -> [a] que toma tres elementos y los
--los convierte en una lista

listar :: a -> a -> a -> [a]
listar p q k = p:q:k:[] 

{-ejemplo de funcionamiento: listar 1 2 3, te devuelve [1,2,3]-} 
------------------------------------------------------------------

------------------------------------------------------------------
{-
  Definir la funcion newPertenece :: Ep a => a -> [a] -> Bool que indica si el elemento
que se le pasa está o no en la lista.
-}
newPertenece :: (Eq a) => a -> [a] -> Bool
newPertenece n lis | length lis == 0 = False
                   | (head lis) == n = True
                   | otherwise = newPertenece n (tail lis)


------------------------------------------------------------------
productoria:: [Integer] -> Integer
productoria ls | length ls == 0 = 1
               | otherwise = (head ls)*productoria (tail ls)
------------------------------------------------------------------

------------------------------------------------------------------
sumarN :: Integer -> [Integer] -> [Integer]
sumarN n ls | length ls == 0 = []
            | otherwise = ((head ls)+n) : (sumarN n (tail ls))
------------------------------------------------------------------



--¿COMO ESCRIBIR LA FUNCION SUMATORIA USANDO PATTERN MATCHING?

sumPattern :: [Integer] -> Integer
sumPattern [] = 0
sumPattern (x:xs) = sumPattern xs + x

{-
    Comentario:

      Las listas también admiten el aptron _ 
      que se corresponde con cualquier valor
      pero no liga ninguna variable
-}


longitud :: [a] -> 
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

--Redefinicion de newPertenece pero en pattern matching
pertein :: (Eq a) => a -> [a] -> Bool
pertein [] = 0


















