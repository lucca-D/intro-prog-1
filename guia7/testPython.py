
def pertenece(s: list[int], e: int) -> bool:
    for i in s:
        if i == e: return True
    return False

def perteneceAcadaUno(s: list[list[int]], e:int) -> list[bool]:
    lista = []
    for i in s:
        if pertenece(i,e): lista.append(True)
        else: lista.append(False)
    return lista
            
def esMatriz(m: list[list]) -> bool:
    if len(m) == 0: return False
    if len(m[0]) == 0: return False
    longitud = len(m[0])
    for i in range(0,longitud):
        if len(m[i]) != longitud: return False
    return True

def columna(m: list[list],c: int) -> list:
    lista: list = []
    for i in range(len(m)):
        lista.append(m[i][c])
    return lista

def bien_formada_tateti(m: list[list[str]]) -> bool:
    return (len(m) == 3 and len(m[0]) == 3)

def existe_fila_de_letra(m: list[list[str]],letra: str) -> bool:
    for i in range(len(m)):
        if(m[i][0] == letra and m[i][1] == letra and m[i][2] == letra): return True
    return False

def existe_columna_de_letra(m: list[list[str]],letra: str) -> bool:
    for c in range(len(m)):
        varAux: list = columna(m, c)
        if(varAux[0] == letra and varAux[1] == letra and varAux[2] == letra): return True
    return False

def existe_diagonal_de_letra():
    print("hola")

def quien_gana_tateti(m: list[list[str]]) -> int:
    if (not esMatriz(m) or not bien_formada_tateti(m)): return 3
    return 2

""" 
4. Programas interactivos usando secuencias
Ejercicio 7. Vamos a elaborar programas interactivos (usando la funcion input()3) que nos permita solicitar al usuario
informacion cuando usamos las funciones.

1. Implementar una funcion para construir una lista con los nombres de mis estudiantes. La funcion solicitara al usuario
los nombres hasta que ingrese la palabra “listo”, o vacio (el usuario aprieta ENTER sin escribir nada). Devuelve la
lista con todos los nombres ingresados.
"""

def armar_listado_alumnos():
    entrada : str = input("Ingrese nombre de alumno: \n")
    listadoDeAlumnos : list = []
    while (entrada != "" and entrada != "listo"):
        listadoDeAlumnos.append(entrada)
        entrada : str = input("Ingrese nombre de alumno: \n")
    print(listadoDeAlumnos)

""" 
 4. Analizar la fortaleza de una contrasena. Solicitar al usuario que ingrese un texto que sera su contrasena. Armar una
 funcion que tenga de parametro de entrada un string con la contrasena a analizar, y la salida otro string con tres
 posibles valores: VERDE, AMARILLA y ROJA. Nota: en python la “ñ/ Ñ” es considerado un caracter especial y no
 se comporta como cualquier otra letra. String es seq⟨Char⟩. Consejo: para ver si una letra es mayuscula se puede ver
 si esta ordenada entre A y Z.
La contraseñna sera VERDE si:
a) la longitud es mayor a 8 caracteres
b) tiene al menos 1 letra minuscula.
c) tiene al menos 1 letra mayuscula.
d) tiene al menos 1 digito numerico (0..9)
La contraseñna sera ROJA si:
a) la longitud es menor a 5 caracteres.
En caso contrario sera AMARILLA.
"""

def tiene_minuscula(contra: str) -> bool:
    for i in range(len(contra)):
        letra: str = contra[i]
        if(ord("a") <= ord(letra) and ord("z") >= ord(letra)) or letra == "ñ": return True
    return False

def tiene_mayuscula(contra: str) -> bool:
    for i in range(len(contra)):
        letra: str = contra[i]
        if(ord("A") <= ord(letra) and ord("Z") >= ord(letra)) or letra == "Ñ": return True
    return False

def tiene_digito(contra: str) -> bool:
    for i in range(len(contra)):
        letra: str = contra[i]
        if ord("0") <= ord(letra) and ord("9") >= ord(letra): return True
    return False

def seguridad_contrasena(contra : str):# -> str:
    if len(contra) < 5: print("ROJA")
    elif len(contra) > 8 and tiene_minuscula(contra) and tiene_mayuscula(contra) and tiene_digito(contra): print("VERDE")
    else: print("AMARILLO")


