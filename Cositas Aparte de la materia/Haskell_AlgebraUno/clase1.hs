-- Comentario : Con las dos barras "--" se realizan los cometarios
--        OBSERVACION:= NO USES EL TABULADOR. A HASKELL NO LE GUSTAN, ASI QUE CUNDO IDELTES
--USA LA BARRA ESPACIADORA

{----------FUNCIONES-------------------------------------------------------------------- 
    Para definir funciones es necesario entender que:
	1) Se definen en el siguiente orden :
                  nombreFuncion parametroUno parametroDos ... parametroN = definicion de la funcion 
	2) Siempre deben respetarse los espacios, ya que en haskell el espacio vacio es un operador.
----------------------------------------------------------------------------------------}

{- ------------------------------------------------------------------------------------------------------
        --Ejercicio 1:
	Explicare solo esta funcion de las basicas. Aca tenes la funcion "f" que recibe a "x" e "y" como parametros.
    Luego, tenemos que lo que va a hacer f es primero multiplicar x por x, luego hace la multiplicacion de y por y 
y despues suma. Esto es porque haskell ya tiene definido un orden de uso para las funciones +,-,*,/. Tambien pueden definirse encerrando las operaciones entre parentesis
 --------------------------------------------------------------------------------------------------------}
f x y = x * x + y * y

--Ejercicio 2:
g x y z = x + y + z * z

--Ejercicio 3:
doble x = 2 * x

--Ejercicio 4:
suma x1 x2 = x1 + x2

--Esto no es un ejercicio
cuadrado x = x*x

--Ejercicio 5:
--Si no le ponias los parentesis no hacia bien la cuenta 
-- sqrt : Funcion raiz cuadrada
normaVectorial x1 x2 = sqrt (x1**2 + x2**2)

--Ejercicio 6:
funcionConstante8 x = 8

--Ejercicio 7:
respuestaATodo = 42
{-
--Ejercicio 8:
     Cuidado, cuando evaluas el negativo hace signo (-n)
Cuando escribis las funciones de esta forma recorda no utilizar Tab

Los pipe "|" delimitan condiciones, casi como un if en otros lenguajes. Se utiliza para definir funciones por partes.
Explicacion de la funcion: la funcion "signo" toma como parametro el valor "n". Ahora, si "n" es cero, entonces la funcion signo nos devuelve cero, insinuando que el numero no tiene signo. Si "n" es mayor que cero, es decir es positivo, nos devuelve 1 y si es negativo  (menor a cero) nos devuelve menos 1.
-}
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
--Esta es una manera de reciclar codigo ya hecho, no es nada mas que usar una funcion ya hecha dentro de otra.
max x y z = maximo ( maximo x y ) z  
---------------
-- PRUEBITAS --
---------------

prueba1 w x y z = div (mod w x) (mod y z)

------------------------------------------------------------------
--     Fijate que las funciones tenes que llamarlas encerradas entre        --
--     Parentecis segun su cantidad de parametros recibidos                          --
------------------------------------------------------------------

normaVectorial2 x y = sqrt (suma (cuadrado x) (cuadrado y)) 
































