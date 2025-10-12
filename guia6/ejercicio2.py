import math
# Ejercicio 2. Definir las siguientes funciones y procedimientos con par ́ametros:

#1. problema imprimir saludo (in nombre: String) {
#requiere: { True }
#asegura: {imprime “Hola < nombre >”por pantalla}
#}

def imprimir_saludo(nombre: str):
    print("Hola","castor",nombre)

#4. problema es multiplo de (in n: Z, in m:Z) {
#requiere: { m 6= 0 }
#asegura: {res = T rue ↔ (∃k : Z)(n = m ∗ k)}
#}

def es_multiplo_de(n: int, m: int) -> bool:
    if((n % m ) == 0):
        return True
    else: 
        return False
    
# 5. es par(numero): que indique si numero es par (usar la funci ́on es multiplo de()).

def es_par(n:int) -> bool:
    return es_multiplo_de(n,2)

# 6. cantidad de pizzas(comensales, min cant de porciones) que devuelva la cantidad de pizzas que necesitamos para
#que cada comensal coma como m ́ınimo mincantdeporciones porciones de pizza. Considere que cada pizza tiene 8
#porciones y que se prefiere que sobren porciones.

def cantidad_de_pisas(comensales:int,min_cant_de_porciones:int) -> int:
    total_de_porciones = comensales * min_cant_de_porciones
    return(math.ceil(total_de_porciones / 8))