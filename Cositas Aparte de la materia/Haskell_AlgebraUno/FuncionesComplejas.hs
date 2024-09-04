module FuncionesComplejas
where
import FuncionesSimples

{---------------Importando Modulos----------------------------------------------------------
1) Las funciones doble y suma usadas en este nuevo modulo son las definidas en FuncionesSimples.hs
2) Si no especificamos cuales funciones exportamos en un módulo, se exporta todo por defecto.

NOTAR QUE: Este archivo tambien es un modulo, ya que esta definido arriba como tal, en este caso
todas las funciones van a poder utilizarse de manera externa.
--------------------------------------------------------------------------------------------}

cuadruple :: Num a => a -> a
cuadruple x = doble (doble x)

sumaTupla :: Num a => (a,a) -> a
sumaTupla t = suma (fst t) (snd t)


