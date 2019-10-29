{-                 Funcion resta: 
 Resive dos parametros de tipo Integer y 
devuelve otro Integer Y mediante el 
operador - se vinculan los parametros para
 obtener el resultado
-}
resta :: Integer -> Integer -> Integer
resta x y = x - y

{-                 Funcion suma: 
 Resive dos parametros de tipo Integer y 
devuelve otro Integer Y mediante el 
operador + se vinculan los parametros para
 obtener el resultado
-}
suma :: Integer -> Integer -> Integer
suma x y = x + y  

{-                 Funcion negar: 
 Resive un parametro de tipo Integer y 
devuelve otro Integer Y mediante el 
operador - se niega el valor de x
-}
negar :: Integer -> Integer 
negar x = - x

{-        CLASIFICACION DE FUNCIONES
      TOTALES: Nunca se indefinen ( Ej : Funcion suc)
       PARCIALES: Existen valores de argumentos para los cuales la funcion no esta definida (Ej: Funcion inv)
-}

{-        FUNCION suc:
Resive un numero x y te devuelve eñ
numero siguiente.
-}
suc :: Integer -> Integer
suc x = x + 1

{-       Funcion inv
Recive un numero y te devuelve su
inverso multiplicativo
-}
inv :: Float -> Float
inv x | x /= 0 = 1/x

{------------}
{-EJERCICIOS-}
{------------}
{------------------------------------------------------------------------
1)>>
 unidades: Dado un entero, devuelve el digito de las unidades del número.  
------------------------------------------------------------------------}
unidades :: Integer -> Integer
unidades x = mod x 10
{------------------------------------------------------------------------
2)>>
 sumaUnidades3: Dados 3 números enteros, devueleve la suma de las
ultimo digito de los tres numeros.
------------------------------------------------------------------------}
sumaUnidades3 :: Integer -> Integer -> Integer -> Integer
sumaUnidades3 x y z = unidades x + unidades y + unidades z

{------------------------------------------------------------------------ 
3)>> 
 todosImpares: Dados 3 números enteros determina si son todos impares.
------------------------------------------------------------------------}
todosImpares :: Integer -> Integer -> Integer -> Bool
todosImpares x y z | mod x 2 + mod y 2 + mod z 2 == 3 = True 
                                         | otherwise = False 
{-----------------------------------------------------------------------
4)>>
 alMenosUnImpar: Dados 3 números eneros determina si al menos 
		uno de ellos es impar. 
-----------------------------------------------------------------------}
alMenosUnImpar :: Integer -> Integer -> Integer -> Bool
alMenosUnImpar x y z | mod x 2 + mod y 2 + mod z 2 == 0 = False
                                            | otherwise = True
{------------------------------------------------------------------------
5)>>
 alMenosDosImpares: Dados 3 numeros enteros determina si al menos 
		    dos de ellos son impares
------------------------------------------------------------------------}
alMenosDosImpares :: Integer -> Integer -> Integer -> Bool
alMenosDosImpares x y z | mod x 2 + mod y 2 + mod z 2 > 1 = True 
                                                   | otherwise = False

{------------------------------------------------------------------------
6)>>
 alMenosDosPares: Dados 3 numeros enteros determina si al menos 
		    dos de ellos son pares
------------------------------------------------------------------------}
alMenosDosPares :: Integer -> Integer -> Integer -> Bool
alMenosDosPares x y z = not (alMenosDosImpares x y z)  

