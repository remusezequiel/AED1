module Clase6
where

factorial 0 = 1
factorial n = n * factorial (n - 1)

{-Todos los tipos de datos admiten el patron "_", que se corresponde con cualquier valor pero no liga
ninguna variable. Lo usamos cuando no nos importa el valor de algun parametro-}

esLaRespuestaATodo :: Integer -> Bool
esLaRespuestaATodo 42 = True
esLaRespuestaATodo _ = False

{-El Pattern matching tambien nos permite escribir de forma mas clara deficiniones que involucren tuplas-}

sumaVectorial :: (Float, Float) -> (Float, Float) -> (Float, Float) 
sumaVectorial (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


{-Esta te va a tirar error de patron, no va a compilar

facto :: Integer -> Integer
facto 0 = 1
facto (n + 1) = (n + 1) * facto n

-}
{- Tira error en la primer definicion, ya que no puedes redefinir la variable en tiempo de compilacion

iguales :: Integer -> Integer -> Bool
iguales x x = True
iguales x y = False
-}

yLogico :: Bool -> Bool -> Bool
yLogico True True = True
yLogico _ _ = False 
