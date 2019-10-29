module Clase4
where

{-----------------RECURSIVIDAD---------------------------
Si queremos definir una funcion recursiva, por ejemplo
"factorial":

1) En el paso recursivo, suponiendo que tenemos el resultado
para el caso anterior:
 ¿Que faltaria para obtener el resultado que quiero?
En el caso del factorial, suponemos ya calculado factorial de 
(n - 1) y lo combinamos multiplicandolo por "n" para lograr
obtener "factorial de n".
2) Ademas, identificamos el o los casos base. 

PROPIEDADES DE UNA FUNCION RECURSIVA:
 - Las llamadas recursivas tienen que acercarse a un caso base,
ya que la recursividad funciona como un metodo inductivo.
- Tiene que tener almenos un caso base que dependeran del 
tipo de llamado recursivo. 
- Un caso base es aquella expresion que no tiene paso recursivo

--------------------------------------------------------}

factorial :: Integer -> Integer
factorial n | n == 0 = 1
           | n > 0 = n  * factorial (n - 1) 

-- Un ejemplo de una funcion recursiva con mas de un caso base necesario es la siguiente:
esPar :: Integer -> Bool
esPar n | n == 0 = True
         | otherwise = not (esPar (n - 1))
{-------¿COMO FUNCIONA ESTO?----------------------------------------
      Esta funcion lo que hace es: 
        Si "n" es cero  => como cero es par devuelve True
        Despues, si "n" no es cero se le pasa como condicion 
Que se niegue esPar n-1, esto es porque si "n" es Impar
Cuando llegue a 0 la condicion final estara negada y asi tambien
su caso base, dando como resultado que si es Impar llamara a la negacion
y sino lo es Sera verdadero, ya que el llamado recursivo de la negacion
da como resultado el caso no negado.
 ------------------------------------------------------------------}

---------------
-- EJERCICIOS --
---------------
{- 1 >>
              Implementar la funcion fib : Enteros positivos -> enteros que devuelve el i-esimo
numero de Fibonnacci.
-}
fib :: Integer -> Integer
fib n 
      | n == 0 = 0
      | n == 1 = 1
      | n == 2 = 2
      |otherwise = fib (n - 2) + fib (n - 1)

{- 2>> 
              Imprelentacion de funciones diversas para calcular el n-esimo termino de 
las sucesiones del ejercicio 16 y 20 de la guia de induccion.
-}
--Funcion potencia para utilizar en eje_uno
potencia :: Integer -> Integer -> Integer
potencia m n | n == 0 = 1
                          | otherwise = m * potencia m (n - 1)
--
-- Recordar que al ser una sucesion solo podemos usar numeros naturales (de 1 en adelante)
eje_uno :: Integer -> Integer
eje_uno n | n == 1 = 2
                    | otherwise = (2 * (n - 1)) * (eje_uno (n - 1)) + ((potencia 2 n) * (factorial (n - 1)))

eje_dos :: Integer -> Integer
eje_dos n | n == 1 = 1
                    | n == 2 = 2
                    | otherwise = (n * eje_dos (n - 1)) + (2 * n * eje_dos (n - 2))

eje_tres :: Integer -> Integer
eje_tres n | n == 1 = - (3)
                       | n == 2 = 6
                       | n > 2 &&  esPar n == True = - eje_tres (n - 1) - 3
                       | n > 2 &&  esPar n == False = eje_tres (n - 1) + (2 * eje_tres (n - 2)) + 9

--Implementacion de la funcion sumatoria

sumatoria :: Integer -> Integer -> Integer
sumatoria i n | n == 1 = i
               | otherwise = n + sumatoria i (n - 1)  
--Ejercicios varios. Ver Ultimos Ejercicios del Pdf
--16
sumaDosAlai :: Integer -> Integer 
sumaDosAlai  n | n == 0 = 1                     
                 | otherwise = 2 ^ n + sumaDosAlai (n - 1)

--17-f2
sumaDeQAlai :: Integer -> Float -> Float
sumaDeQAlai n q | n == 1 = q
                   | otherwise = q ^ n + sumaDeQAlai (n - 1) q 

--18-f3
sumaDeQAlaiHasta2Ala_n :: Integer -> Float -> Float
sumaDeQAlaiHasta2Ala_n n q = sumaDeQAlai (2*n) q 

--19-f4
f4 :: Integer -> Float -> Float
f4 n q | n == 1 = sumaDeQAlaiHasta2Ala_n n q
       | otherwise = sumaDeQAlaiHasta2Ala_n n q - sumaDeQAlai (n - 1) q 
























 





