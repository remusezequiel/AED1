module Clase10
where


type Polinomio = [Float]
type Complejo = (Float, Float)


grado :: Polinomio -> Integer
grado [x] = 0
grado (x:xs) = 1 + grado xs

evaluar :: Polinomio -> Float -> Float
evaluar [] h = 0
evaluar (x:xs) h = x * h ^ grado (x:xs) + evaluar xs h

derivada :: Polinomio -> Polinomio
derivada [] = []
derivada [x] = []
derivada (x:xs) = (fromInteger (grado (x:xs)) * x):(derivada (xs))

derivadaN :: Integer -> Polinomio -> Polinomio
derivadaN 0 (x:xs) = (x:xs)
derivadaN n (x:xs) = derivada (derivadaN (n - 1) (x:xs)) 

sumaPol :: Polinomio -> Polinomio -> Polinomio
sumaPol [] [] = []
sumaPol [] [x] = [x]
sumaPol [x] [] = [x]
sumaPol (x:xs) (y:ys) | grado (x:xs) == grado (y:ys) = (x + y) : sumaPol (xs) (ys) 
                      | grado (x:xs) > grado (y:ys) = x : sumaPol (xs) (y:ys) 
                      | grado (x:xs) < grado (y:ys) = y : sumaPol (x:xs) (ys)

productoPorEscalar :: Float -> Polinomio -> Polinomio
productoPorEscalar 0 (x:xs) = []
productoPorEscalar f [] = []
productoPorEscalar f (x:xs) = x * f : productoPorEscalar f xs


sumAux :: Polinomio -> Polinomio -> [Float]
sumAux [] q = q
sumAux p [] = p
sumAux p q = (sumAux (init p) (init q)) ++ [last (p) + last (q)] 

limpiar :: [Float] -> Polinomio
limpiar [] = []
limpiar (x:xs) | x /= 0 = (x:xs)
               | otherwise = limpiar xs

suma :: Polinomio -> Polinomio -> Polinomio
suma p q = limpiar (sumAux p q)


productoPorMonomio :: (Float, Integer) -> Polinomio -> Polinomio
productoPorMonomio (0,_) _ = []
productoPorMonomio (a,0) p = productoPorEscalar a p 
productoPorMonomio (a,m) p = (productoPorMonomio (a,m-1) p) ++ [0]

productoDePolinomios :: Polinomio -> Polinomio -> Polinomio 
productoDePolinomios [] q = []
productoDePolinomios (x:xs) q = suma (productoPorMonomio (x,grado (x:xs)) q) (productoDePolinomios (xs) (q))


sumaComplejo :: Complejo -> Complejo -> Complejo
sumaComplejo (a,b) (c,d) = (a+b, c+d)

productoComp :: Complejo -> Complejo -> Complejo
productoComp (a,b) (c,d) = ((a * c) - (b * d), (a * d) + (b * c))

potenciaComp :: Complejo -> Integer -> Complejo
potenciaComp _ 0 = (1, 0)
potenciaComp z m = productoComp z (potenciaComp z (m - 1)) 
{- 
evaluarComplejo :: Polinomio -> Complejo -> Complejo
evaluarComplejo [] _ = (0, 0)
evaluarComplejo (x:xs) z = sumaComplejo (productoComp (x, 0) (potenciaComp z (grado (x:xs)))) (evaluarComplejo (limpiar xs) z)
-}











