-- Comentario : Con las dos barras "--" se realizan los cometarios

--Ejercicio 1:
f x y = x * x + y * y

--Ejercicio 2:
g x y z = x + y +z * z

--Ejercicio 3:
doble x = 2 * x

--Ejercicio 4:
suma x1 x2 = x1 + x2

--Esto no es un ejercicio
cuadrado x = x*x

--Ejercicio 5:
--Si no le ponias los parentesis no hacia bien la cuenta 
normaVectorial x1 x2 = sqrt (x1**2 + x2**2)

--Ejercicio 6:
funcionConstante8 x = 8

--Ejercicio 7:
respuestaATodo = 42

--Ejercicio 8:
--Cuidado, cuando evaluas el negativo hace signo (-n)
--Cuando escribis las funciones de esta forma recorda no utilizar Tab
signo n | n == 0 = 0 
        | n > 0 = 1 
        | n < 0 = -1

--Ejercicio 9:
absoluto n | n > 0 = n 
           | n <= 0 = -n

absoluto2 n = n * signo n

absoluto3 n = sqrt ( n ^ 2 )
           
--Ejercicio 10:
maximo x y | x > y = x  
           | otherwise = y

--Ejercicio 11:
maximo3 x y z | x > y && x > z = x 
              | y > x && y > z = y 
              | z > x && z > y = z

max x y z = maximo ( maximo x y ) z  
---------------
-- PRUEBITAS --
---------------

prueba1 w x y z = div (mod w x) (mod y z)

------------------------------------------------------------------
-- Fijate que las funciones tenes que llamarlas encerradas entre--
-- Parentecis segun su cantidad de parametros recibidos         --
------------------------------------------------------------------

normaVectorial2 x y = sqrt (suma (cuadrado x) (cuadrado y)) 
































