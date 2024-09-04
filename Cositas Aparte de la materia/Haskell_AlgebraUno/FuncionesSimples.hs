module FuncionesSimples 
(suma, doble)
where

{-----------------MODULOS--------------------------------
     Este archivo sirve como ejemplo de creacion de modulos.
  La idea es encapsular codigo en un archivo y poder utilizar
el modulo en otro archivo para reutilizar las funciones 
escritas en el modulo por medio de la llamada "import" desde 
otro archivo.

Para crear un modulo, solo hace falta definir el modulo 
en la parte de arriba con la palabra "module" y pasandole el 
nombre del archivo tal cual. 

OJO!: El archivo debe tener su primer letra en mayusculas
ademas te tener la extencion .hs

Lego de esto, entre parentesis de pasamos las funciones que 
queremos  que se puedan usar por afuera de este modulo, donde 
si queremos que se puedan usar todas no le decimos nada.

Por ultimo, declaramos la sentencia "where" que le da la pauta 
a module de que las funciones son las que estan debajo 
de "where". 

Este archivo va a estar llamado en el archivo:
                                                                             "FuncionesComplejas.hs"
---------------------------------------------------------}

suma :: Num a => a -> a -> a
suma x y = x + y

doble :: Num a => a -> a
doble x = 2 * x

triple ::  Num a => a -> a
triple x = 3 * x
