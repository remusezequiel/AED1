module Clase8
where
import Clase7

type Set a = [a]

vacio :: Set Integer
vacio = []

agregar :: Integer -> Set Integer -> Set Integer
agregar x xs | pertenece x xs = xs
             | otherwise = (x:xs)

incluido :: Set Integer -> Set Integer -> Bool
incluido x xs | length xs < length x = False 
              | length x == 0 = True
              | otherwise = pertenece (head x) xs && incluido (tail x) xs

iguales :: Set Integer -> Set Integer -> Bool
iguales x xs = incluido x xs && incluido xs x

perteneceC :: Set Integer -> Set (Set Integer) -> Bool
perteneceC x [] = False
perteneceC x (y:ys)| iguales x y = True
                   | otherwise = perteneceC x ys

agregarC :: Set Integer -> Set (Set Integer) -> Set (Set Integer)
agregarC x xs | perteneceC x xs = xs
              | otherwise = (x:xs)

agregarATodos :: Integer -> Set (Set Integer) -> Set (Set Integer)
agregarATodos x [] = []
agregarATodos x (xs:ys) | pertenece x xs  = xs:agregarATodos x ys
                        | not(pertenece x xs)  = agregarC (agregar x xs) (agregarATodos x ys)

partes :: Integer -> Set (Set Integer)
partes 0 = []
partes 1 =[[],[1]]
partes n = agregarATodos n (partes (n-1)) ++ partes (n-1)

union :: Eq a => Set a -> Set a -> Set a
union [] ys = ys
union (x:xs) ys | elem x ys = union xs ys
                | otherwise = union xs (x:ys)

duplas :: Set Integer -> Integer -> Set (Integer, Integer)
duplas [] y = []
duplas (x:xs) y = (x,y) : duplas xs y

productoCartesiano :: Set Integer -> Set Integer -> Set (Integer, Integer)
productoCartesiano xs (y:ys) = union (duplas xs y) (productoCartesiano xs ys)
productoCartesiano xs [] = []

prefijarA :: Integer -> Set [Integer]-> Set [Integer]
prefijarA x [] = []
prefijarA x (y:ys) = union [(x:y)] (prefijarA x ys)

prefijarC :: Set Integer-> Set [Integer] -> Set [Integer]
prefijarC [] ys = []
prefijarC (x:xs) ys = union (prefijarA x ys) (prefijarC xs ys) 

variaciones :: Set Integer -> Integer -> Set [Integer]
variaciones xs 0 = [[]]
variaciones xs n = prefijarC xs (variaciones xs (n-1))

