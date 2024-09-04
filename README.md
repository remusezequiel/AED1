# Algoritmos y Estructuras de Datos 1

![Logo de Haskell](https://upload.wikimedia.org/wikipedia/commons/1/1c/Haskell-Logo.svg "Haskell Logo")

## Resumen:

Este repositorio se corresponde con lo visto  la materia **AED 1** para  *Ciencias de la computación* y *Ciencia de Datos* de la UBA. En este se dan las bases de la programacion en (*[Haskell][1]*) para la 1er parte de la materia, el cual es un lenguage de programación funcional basado funciones lambda. Luego la segunda parte se enseñan las bases de los lenguajes imperativos utilizando python. 

## Instalacion Haskell:

GHCI (The Glasgow Haskell Compiller Interactive enviroment)
-----------------------------------------------------------
1. **Ubuntu** : *sudo apt-get install ghc*

2. **Mac** : *brew install haskell-platform*

3. **Windows** : (*[https://www.haskell.org/platform/windows.html][2]*)
-----------------------------------------------------------

## ¿Como usar lo que esta en el repo?:

Lo que tenes que hacer para poder ejecutar las funciones de cada archivo es: 

* Primero: 
  *  Te paras sobre la carpeta donde estan los archivos y abris una terminal
* Segundo:
  *  En la terminal escribis *"ghci"*. Estao te abre el prelude de Haskell.
* Tercero:
  *  Llamas al archivo con el comando *:l*. Ejemplo: *:l Clase1.hs*
* Cuarto: 
  *  Ejecutas las funciónes

> **Cuidado!**. Tenes que tener en cuenta que para Haskell un espacio en blanco es un operador. Luego, para llamar por ejemplo a la función *suma*, la cual recibe dos parametros, en la terminal deberas llamarla *suma 3 4*, luego este de hara la suma y te tira como resultado 7 debajo, pero es incorrecto si escribes *suma 34*. Lo mismo si haces *3+4* pegado en el prelude, debes escribirlo de la siguiente forma *3 + 4*. Por otro lado, cuando usamos numeros negativos en una función, debemos siempre encerrarla entre parentesis, sino no va a correr.



[1]: https://www.haskell.org
[2]: https://www.haskell.org/platform/windows.html