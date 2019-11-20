union :: Eq a => [a] -> [a] -> [a]
union [] ys = ys
union (x:xs) ys | elem x ys = union xs ys
                | otherwise = union xs (x:ys)

--Crea una lista de 1 hasta n
lisN :: Integer -> [Integer]
lisN n = [x | x <- [1..n]]

--Crea una lista de 1 hasta n
listaDeN :: Integer -> [Integer]
listaDeN 1 = union [1] []                              
listaDeN 2 = union [2] [1]  
listaDeN n | (longitud (union [n-1] [n]) < n) = union [n] [] ++ listaDeN (n - 1)

longitud :: [Integer] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs


--Crea una lista 
crearLista :: Integer -> [Integer]
crearLista n = listaInversa (listaDeN n)

--Realiza la inversa de la lista             
listaInversa :: [Integer] -> [Integer]
listaInversa [] = []
listaInversa (x:xs) = listaInversa xs ++ [x]


-- intercala: lo que hace es intercalar 1 numero la cantidad de veces que pueda en la lista formada 
-- si el numero se agrega a una lista vacia, hace un conjunto que contiene a la lista [n]
-- Si el numero n se agrega a una lista de 3 elementos, se pondra en los 4 lugares posibles el primero, el segundo, el tercero y el cuarto lugar
intercala :: a -> [a] -> [[a]]
intercala x [] = [[x]]
intercala x (y:ys) = (x:y:ys) : [y:zs | zs <- intercala x ys]

--lo que hace es concatenar todos los elementos que tienen las listas dentro de la lista
--Probalo corriendo: concatenar [[1,2,3], [4,5,6], [7,8,9]]
concatenar :: [[a]] -> [a]
concatenar xss = [x | xs <- xss, x <- xs]

--Lo que hace es concatenar un llamado recursivo a los elementos de la cola de la lista
--pasada como parametro realizando la operacion intercala, donde x sera la cabeza de la lista
--y ys sera el llamado recursivo a paraPermutaciones 
paraPermutaciones :: [a] -> [[a]]
paraPermutaciones []     = [[]]
paraPermutaciones (x:xs) = concatenar [intercala x ys | ys <- paraPermutaciones xs]

--Lo que hace esta funcion es crear una lista de n elementos y a esa lista la permuta
--segun las letras de parapermutaciones
permutaciones :: Integer -> [[Integer]]
permutaciones n = paraPermutaciones (crearLista n)
