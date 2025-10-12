import math
# Ejercicio 3. Resuelva los siguientes ejercicios utilizando los operadores l ́ogicos and, or, not. Resolverlos sin utilizar
#alternativa condicional (if).

#1. alguno es 0(numero1, numero2): dados dos n ́umeros racionales, decide si alguno de los dos es igual a 0.

def peso_pino(altura:int) -> int:   # recibe altura del pino en metros
    if(altura > 2):
        peso = 600 + (altura-2)*200
    else:
        peso = altura*300
    return peso

def es_peso_util(peso:int) -> bool:
    return peso>400 and peso<1000

def sirve_pino(altura:int):
    return es_peso_util(peso_pino(altura))

print(sirve_pino(1))
print(sirve_pino(2))
print(sirve_pino(3))
print(sirve_pino(4))