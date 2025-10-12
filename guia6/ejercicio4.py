# Ejercicio 4. Usando las funciones de python min y max resolver:
# En una plantación de pinos, de cada árbol se conoce la altura expresada en metros. 
# El peso de un pino se puede estimar a partir de la altura de la siguiente manera:
# - 3 kg por cada centímetro hasta 3 metros,
# - 2 kg por cada centímetro arriba de los 3 metros.

# Por ejemplo:
# - Un pino de 2 metros pesa 600 kg, porque 200 * 3 = 600.
# - Un pino de 5 metros pesa 1300 kg, porque los primeros 3 metros pesan 900 kg 
#   y los siguientes 2 metros pesan los 400 restantes.

# Los pinos se usan para llevarlos a una fábrica de muebles, a la que le sirven árboles 
# de entre 400 y 1000 kilos. Un pino fuera de este rango no le sirve a la fábrica.

# Definir las siguientes funciones, deducir qué parámetros tendrán a partir del enunciado. 
# Se pueden usar funciones auxiliares si fuese necesario para aumentar la legibilidad.

# 1. Definir la función peso_pino.
# 2. Definir la función es_peso_util, que recibe un peso en kg y responde si un pino de ese peso 
#    le sirve a la fábrica.
# 3. Definir la función sirve_pino, que recibe la altura de un pino y responde si un pino de ese peso 
#    le sirve a la fábrica.
# 4. Definir sirve_pino usando composición de funciones.

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

