--clase 08 miércoles 2 de octubre de 2019

dividir :: Integer -> Integer -> Integer
dividir a b | a < b = 0
            |otherwise = 1 + dividir (a-b) b --aplicamos la idea de acercarnos al caso base

longitud :: [Integer] -> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

quitarT :: Integer ->[Integer] -> [Integer]
quitarT n [] = []
quitarT n (x:xs) | n == x = quitarT n xs
                 | otherwise = x:(quitarT n xs)

--hayRepetidos :: [Integer] -> Bool
--hayRepetidos [] = False
--hayRepetidos (x:xs) | pertenece x xs = True
  --                  | otherwise = hayRepetidos xs

--eliminarRepetidos :: [Integer] -> [Integer]
--eliminarRepetidos [] = []
--eliminarRepetidos (x:xs) |pertenece x xs = eliminarRepetidos xs
 --                        | otherwise = x : eliminarRepetidos xs

--maximo :: [Integer] -> [Integer]
--maximo xs | length xs == 1 = head xs
   --       | otherwise = max (head xs) maximo tail xs

--maximoPM :: [Integer]->[Integer]
--maximoPM [x] = x
--maximoPM (x:xs)= max (maximo xs)

--ordenar :: [Integer] -> Integer
--           |otherwise = (minimo xs): ordenar (quitar) (minimos xs) xs)

--minimo [x] = x
--minimo (x:xs)= min x (maximo xs)

reverso :: [Integer] -> [Integer] --hasta acá fue repaso y ejercicios de la clalse anterior, la clase 7
reverso [] = []
reverso (x:xs) = reverso xs ++[x]

--agregar :: Integer -> Set Integer -> Ser Integer
--             | otherwise = x:xs

type Set a =[a]

vacio :: Set Integer
vacio = []

agregar :: Eq a => a -> Set a -> Set a
agregar x xs | elem x xs = xs
             |otherwise = x:xs

incluido :: Set Integer -> Set Integer -> Bool
incluido [] _ = True
--incluido _ [] = False
incluido (x:xs) ys = elem x ys && incluido xs ys


iguales :: Set Integer -> Set Integer -> Bool
iguales xs ys = incluido xs ys && incluido ys xs

--agregarC :: Set Integer -> Set (Set Integer) -> Set (Set Integer)
--agregarC xs ys | elem [xs] ys = ys 
          --     | otherwise = [xs]++ys

pertenece2 :: Set Integer -> Set (Set Integer) -> Bool
pertenece2 x [] = False
pertenece2 x (y:ys) | iguales x y = True
                    | otherwise = pertenece2 x ys

agregarC :: Set Integer -> Set (Set Integer) -> Set (Set Integer)
agregarC x xs | pertenece2 x xs = xs
              | otherwise = x:xs

agregarATodos :: Integer -> Set (Set Integer) -> Set (Set Integer)--resolvemos en clase 09, era del final de la clase 08
agregarATodos n xs | xs == [] = []
                   | otherwise = agregarC (agregar n (head xs)) (agregarATodos n (tail xs)) 
                   
partes :: Integer -> Set (Set Integer)
partes 0 = [[]]
partes n = partes (n-1)++ agregarATodos n (partes(n-1))

union :: Eq a => Set a -> Set a -> Set a --ejercicio de clase 9, clase antes del parcial, lo pude hacer con ayuda
union xs ys | xs == [] = ys
            | ys == [] = xs
            | elem (head xs) ys == True = union (tail xs) ys
            | otherwise = head xs:(union (tail xs) ys)

unionPM :: Eq a => Set a -> Set a -> Set a --hecho por Pablo con Pattern Matching
unionPM [] ys = ys
unionPM (x:xs) ys | elem x ys = unionPM xs ys 
                  | otherwise = unionPM xs (x:ys)

productoCartesiano :: Set Integer -> Set Integer -> Set (Integer,Integer) --echo en clase, no lo pude hacer
productoCartesiano [x] ys = unoConTodos x ys
productoCartesiano (x:xs) ys = unionPM (unoConTodos x ys) (productoCartesiano xs ys)

unoConTodos :: Integer -> Set Integer -> Set (Integer,Integer)
unoConTodos n [x] = [(n,x)]-- [x] quiere decir que tiene un elemento, siempre se le pasa un conjunto que es no vacío, es u na lista de tuplas
unoConTodos n (x:xs) = (n,x):(unoConTodos n xs)

variaciones :: Set Integer -> Integer -> Set [Integer]-- separamos el problema en partes
variaciones (x:xs) 0 = [[]]--representa el conjunto con la lista vacia
variaciones xs n = prefijarCTodas xs (variaciones xs (n-1))


--prefijarATodas :: Integer -> Set [Integer] -> Set [Integer]
--prefijarATodas n [] = [[n]], está mal, no se le puede agregar algo a [], pero sí a [[]]
--prefijarATodas n xs = prefijarATodas

prefijarATodas2 :: Integer -> Set [Integer] -> Set [Integer]
prefijarATodas2 n [] = []
prefijarATodas2 n (x:xs) = union [(n:x)] (prefijarATodas2 n xs)

prefijarCTodas :: Set Integer -> Set [Integer] -> Set [Integer]
prefijarCTodas [] ys = [] --pregunta buena, ¿por qué no va ys?
prefijarCTodas (x:xs) ys = union (prefijarATodas2 x ys) (prefijarCTodas xs ys)




