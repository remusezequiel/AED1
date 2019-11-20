{-----------------------------
 - * NUMERO DE GRUPO : 32
 -
 - * NOMBRES: 
 -        BONINO BIANCA
 -        CONTRERAS SANTIAGO
 -        REMUS EZEQUIEL
 -----------------------------}


type Circulo = [Integer]

--------------------------
--1: sonCirculosIguales
--------------------------
---Funciones Auxiliares necesarias
{-   longCirculos
 -
 - Toma un circulo y cuenta la cantidad de elementos
 -}
longCirculos :: Circulo -> Integer
longCirculos [] = 0
longCirculos (x:xs) = 1 + longCirculos xs

{- circulosIdenticos
 -
 - Toma dos circulos como parametros y si son
 - elemento a elemento son iguales devuelte True
 -}
circulosIdenticos :: Circulo -> Circulo -> Bool
circulosIdenticos [] [] = True
circulosIdenticos (x:xs) (y:ys) | x == y = circulosIdenticos xs ys
                                | otherwise = False

{- circulosIgualesAux
 -
 - Similar a la anterior solo que si elemento a elemento 
 - no son iguales, entonces rota el segundo circulo
 -}
circulosIgualesAux :: Circulo -> Circulo -> Bool
circulosIgualesAux (x:xs) (y:ys) | x==y = circulosIdenticos xs ys
                                 | otherwise = circulosIgualesAux (x:xs) (ys ++ [y])

--Funcion sonCirculosIguales--
{-Utiliza la funcion circulosIgualesAux para conocer si son circulos iguales teniendo en cuenta las rotaciones
Esto lo hace, si y solo si los circulos tienen igual longitud-}
sonCirculosIguales :: Circulo -> Circulo -> Bool
sonCirculosIguales [] [] = True
sonCirculosIguales xs ys | longCirculos xs == longCirculos ys = circulosIgualesAux xs ys 
                         | otherwise = False

--------------------------
--2: permutaciones
--------------------------
--Funciones Auxiliares necesarias
{-  cambiarAux  
 - 
 -  Cambia al primer numero n coincidente con la cabeza
 -  de la lista y lo cambia por el entero m 
 -}
cambiarAux :: Integer -> Integer -> [Integer] -> [Integer]
cambiarAux _ _ [] = []
cambiarAux n m (x:xs) | n==x = m:xs
                      | otherwise = x: (cambiarAux n m xs)

{-   cambiar
 -
 - Utiliza cambiarAux para cambiar al primer numero n
 - que encuentra en las listas por el numero m. Solo 
 - cambia el primer n, si existen 2 n solo cambia el primero
 -}
cambiar :: Integer -> Integer -> [[Integer]] -> [[Integer]]
cambiar _ _ [] = []
cambiar n m (x:xs) = cambiarAux n m x : cambiar n m xs

{-   agregar
 -
 - Toma el numero n y lo agrega en la cabeza de cada lista
 -}
agregar :: Integer -> [[Integer]] -> [[Integer]]
agregar _ [] = []
agregar n (x:xs) = [n:x] ++ agregar n xs

{-   permutacionesAux
 - 
 - Agrega el elemento n primero a la lista y lo une con 
 - la llamada recurciva pasandose el numero anterior a n 
 - y la lista obtenida tras cambiar al numero anterior a 
 - n por n en la lista pasada como parametro originalmente. 
 -}
permutacionesAux :: Integer -> [[Integer]] -> [[Integer]]
permutacionesAux 0 _ = []
permutacionesAux n xs = agregar n xs ++ permutacionesAux (n-1) (cambiar (n-1) n xs)

{-   permutaciones
 -
 - Realiza una llamada de permutacionesAux de n pasandose como 
 - parametro de lista a la llamada recursiva de permutaciones (n-1)
 - dando una llamada recursiva de permutacionesAux n (permutacionesAux (n-1)) 
 -}
permutaciones :: Integer -> [[Integer]]
permutaciones 1 = [[1]]
permutaciones n = permutacionesAux n (permutaciones (n-1))
--------------------------
--3: esCirculoPrimo
--------------------------
--Funciones Auxiliares necesarias

{-   divisoresHasta
 -
 -   Forma una lista con los divisores del numero pasado
 -  como primer parametro diviviendo por todos los enteros
 -  desde 1 hasta i (aegundo parametro). Retornando dicha lista de divisores
 -}
divisoresHasta :: Integer -> Integer -> [Integer]
divisoresHasta _ 1 = [1]
divisoresHasta n i | mod n i == 0 = i:(divisoresHasta n (i-1))
                   | otherwise = divisoresHasta n (i-1)

{-  esPrimo
 - 
 - Se fija si el entero n es un numero primo o no
 -}
esPrimo :: Integer -> Bool
esPrimo n = length (divisoresHasta n n) == 2

{- circuloPrimoAux 
 -
 - Se fija si el elemento del primer parametro 
 - sumado a los elementos del circulo dan un 
 - numero primo. 
 -}
circuloPrimoAux :: Integer -> Circulo -> Bool
circuloPrimoAux y (x:[]) = esPrimo (y+x)
circuloPrimoAux y (x:xs) | esPrimo (x + head xs) = circuloPrimoAux y xs
                         | otherwise = False

--Funcion esCirculoPrimo--
{-  esCirculoPrimo
 - Se fija si es un circulo primo utilizando la
 - funcion circuloPrimoAux
-}
esCirculoPrimo :: Circulo -> Bool
esCirculoPrimo (x:xs) = circuloPrimoAux x (x:xs)

--------------------------
--4: estaRepetidoPrimero
--------------------------
--Funciones Auxiliares necesarias
{-  estaRepetidoPrimeroAux 
 - 
 - Se fija si el circulo y esta repetido 
 - dentro de una lista de circulos 
-}
estaRepetidoPrimeroAux :: Circulo -> [Circulo] -> Bool
estaRepetidoPrimeroAux y (x:[]) = sonCirculosIguales y x
estaRepetidoPrimeroAux y (x:xs) | sonCirculosIguales y x = True
                              | otherwise = estaRepetidoPrimeroAux y xs
--Funcion esCirculoPrimo--
{-  estaRepetidoPrimero
 - 
 - Utiliza la funcion estaRepetidoAux con
 - el fin de conocer si el primer elemento de 
 - una lista de Circulos esta repetrido dentro de esta
-}
estaRepetidoPrimero :: [Circulo] -> Bool
estaRepetidoPrimero (x:[]) = False
estaRepetidoPrimero (x:xs) = estaRepetidoPrimeroAux x xs

--------------------------
--5: listaCirculosPrimos
--------------------------
--Funciones Auxiliares necesarias
{-
  --------Descripcion
  Esta funcion se fija dentro de una lista de circulos cuales de ellos
  cumplen con ser un circulo primo
 -}
listaCircAux :: [Circulo] -> [Circulo]
listaCircAux [] = []
listaCircAux (x:xs) | estaRepetidoPrimero (x:xs) || not (esCirculoPrimo x) = listaCircAux xs
                    | otherwise = x : listaCircAux xs
--Funcion listaCirculosPrimos--
{-  listaCirculosPrimos
 - 
 - Esta función recive un entero que representa el orden
 - de los circulos que conformaran a la lista. Estos circulos 
 - seran todas las permutaciones posibles de orden n, dentro 
 - de este se evalua cuales son primos y se los 
 - guarda en una lista que sera el retorno de la función.
 -}
listaCirculosPrimos :: Integer -> [Circulo]
listaCirculosPrimos n = listaCircAux (permutaciones n)

--------------------------
--6: contarCirculosPrimos
--------------------------
--Funciones Auxiliares necesarias
{- longitud1 
 -
 - Esta funcion cuenta la cantidad de circulos 
 - dentro de una lista de circulos
 -}
longitud1 :: [Circulo] -> Integer
longitud1 [] = 0
longitud1 (x:xs) = 1 + longitud1 xs

--Funcion contarCirculosPrimos--
{-   contarCirculosPrimos
 - 
 - Esta función cuenta la cantidad de
 - circulos primos de orden n
 -}
contarCirculosPrimos :: Integer -> Integer
contarCirculosPrimos n = longitud1 (listaCirculosPrimos n)

--------------------------
--7: Optativos
--------------------------
{-    ultimoElemento
 -
 - Toma un circulo y retorna el ultimo elemento de este
 -}
ultimoElemento :: Circulo -> Integer
ultimoElemento (x:[]) = x
ultimoElemento (x:xs) = ultimoElemento xs

{-   circuloInverso
 - 
 - Devuelve el circulo inverso al pasado como parametro
 -  Ejemplo: [1,2,3] => [3,2,1]-}
circuloInverso :: Circulo -> Circulo
circuloInverso [] = []
circuloInverso (x:xs) = circuloInverso xs ++ [x]

{-   circulosEspejadosAux
 - 
 - Te dice si un circulo es el inverso del otro o si cumplen 
 - con la condicion de ser espejados por simetria.
 - En particular, el entero pasado como parametro debe ser el 
 - ultimo elemento de la segunda lista
 -}
circulosEspejadosAux :: Integer -> Circulo -> Circulo -> Bool
circulosEspejadosAux u (x:xs) ys | x == u = circulosIdenticos (x:xs) (circuloInverso ys)
                                 | otherwise = circulosEspejadosAux u (xs ++ [x]) ys

{-   sonCirculosEspejados
 -
 - Esta funcion se fija si los circulos pasados como parametro
 - cumplen con las condiciones de ser circulos espejo
 -}
sonCirculosEspejados :: Circulo -> Circulo -> Bool
sonCirculosEspejados xs ys | longCirculos xs == longCirculos ys = circulosEspejadosAux (ultimoElemento ys) xs ys 
                           | otherwise = False
---
{- sonCirculosIgualesOEspejados
 -
 - Esta funcion se fija si los circulos pasados como 
 - parametro seon iguales o espejados
 -}
sonCirculosIgualesOEspejados :: Circulo -> Circulo -> Bool
sonCirculosIgualesOEspejados xs ys = sonCirculosIguales xs ys || sonCirculosEspejados xs ys 
---
{-  repetidoPrimeroEspejadosAux
 -
 - Si fija si el circulo pasado como parametro esta dentro de la lista,
 - pudiendo ser este un espejo de otro dentro de la lista
 -}
repetidoPrimeroEspejadosAux :: Circulo -> [Circulo] -> Bool
repetidoPrimeroEspejadosAux y (x:[]) = sonCirculosIgualesOEspejados y x
repetidoPrimeroEspejadosAux y (x:xs) | sonCirculosIgualesOEspejados y x = True
                                     | otherwise = repetidoPrimeroEspejadosAux y xs

{-estaRepetidoPrimeroEspejados
 -
 - Se fija si el primer elemento de la lista de circulos
 - esta repetido teniendo en cuenta que puede estar espejado
 -}
estaRepetidoPrimeroEspejados :: [Circulo] -> Bool
estaRepetidoPrimeroEspejados (x:[]) = False
estaRepetidoPrimeroEspejados (x:xs) = repetidoPrimeroEspejadosAux x xs
---
{- lisCircPrimosEspejadosAux
 -
 - Se fija dentro de los elementos de la lista de circulos
 - cuelaes estan repetidos o no son primos y los descarta, 
 - dejando solo la lista de circulos primos de la lista  
 -}
lisCircPrimosEspejadosAux :: [Circulo] -> [Circulo]
lisCircPrimosEspejadosAux [] = []
lisCircPrimosEspejadosAux (x:xs) | estaRepetidoPrimeroEspejados (x:xs) || not (esCirculoPrimo x) = lisCircPrimosEspejadosAux xs
                                     | otherwise = x : lisCircPrimosEspejadosAux xs

{- listaCirculosPrimosEspejados
 -
 - Esta función toma un entero como parametro que refleja 
 - el orden de los circulos de la lista a tener en cuenta, 
 - compara si alguno es primo espejado y devuelve la lista 
 - solo con dichos elementos
 -}
listaCirculosPrimosEspejados :: Integer -> [Circulo]
listaCirculosPrimosEspejados n = lisCircPrimosEspejadosAux (permutaciones n)

-------------------
--Funcion opcional
-------------------
{-  contarCirculosPrimosEspejados
 -
 - Esta funcion cuenta la cantidad de elementos de orden
 - n que son primos espejados
 -}
contarCirculosPrimosEspejados :: Integer -> Integer
contarCirculosPrimosEspejados n = longitud1 (listaCirculosPrimosEspejados n)














