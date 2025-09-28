{--
Ejercicio 2. Definir las siguientes funciones sobre listas:
--}

{-- 1. pertenece :: (Eq t) => t-> [t]-> Bool seg´un la siguiente especificaci´on:
 problema pertenece (e: T, s: seq⟨T⟩) : B {
 requiere: { True }
 asegura: { resultado = true ↔ e ∈ s }
 }
--}

pertenece :: (Eq t) => t-> [t]-> Bool
pertenece x [] = False
pertenece x (y:ys) = x == y || pertenece x ys

{-- 2. todosIguales :: (Eq t) => [t]-> Bool, que dada una lista devuelve verdadero s´ı y solamente s´ı todos sus ele
mentos son iguales. --}

todosIguales :: (Eq t) => [t]-> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:xs) = x == head xs && todosIguales xs

{-- 3. todosDistintos :: (Eq t) => [t]-> Bool seg´un la siguiente especificaci´on:
 problema todosDistintos (s: seq⟨T⟩) : B {
 requiere: { True }
 asegura: { resultado = false ↔ existen dos posiciones distintas de s con igual valor }
 }--}

todosDistintosAux :: (Eq t) => t -> [t] -> Bool
todosDistintosAux y [z] =       | y == z = False
                                | otherwise = True
todosDistintosAux y (z:zs) =    | y == z = False
                                | otherwise = todosDistintosAux y zs

todosDistintos :: (Eq t) => [t]-> Bool
todosDistintos [] = True
todosDistintos [x] = True
todosDistintos (x:xs) = todosDistintosAux x xs && todosDistintos xs

{-- 4. hayRepetidos :: (Eq t) => [t]-> Bool seg´un la siguiente especificaci´on:
 problema hayRepetidos (s: seq⟨T⟩) : B {
 requiere: { True }
 asegura: { resultado = true ↔ existen dos posiciones distintas de s con igual valor }
 }--}

hayRepetidosAux :: (Eq t) => t -> [t]-> Bool
hayRepetidosAux y [z]       | y == z = True
                            | otherwise = False
hayRepetidosAux y (z:zs)    | y == z = True
                            | otherwise = hayRepetidosAux y zs

hayRepetidos :: (Eq t) => [t]-> Bool
hayRepetidos [] == False
hayRepetidos [x] == False
hayRepetidos (x:xs) = hayRepetidosAux x xs || hayRepetidos xs

{--5. quitar :: (Eq t) => t-> [t]-> [t], que dados un entero x y una lista xs, elimina la primera aparici´on de x en
 la lista xs (de haberla)--}

quitar :: (Eq t) => t-> [t]-> [t]
quitar x [] = []
quitar x [y]    | x == y = []
                | otherwise = [y]
quitar x (y:ys) | x == y = ys
                | otherwise = y : quitar x ys

{-- 6. quitarTodos :: (Eq t ) => t-> [t]-> [t], que dados un entero x y una lista xs, elimina todas las apariciones
 de x en la lista xs (de haberlas). Es decir:
 problema quitarTodos (e: T, s: seq⟨T⟩) : seq⟨T⟩ {
 requiere: { True }
 asegura: { resultado es igual a s pero sin el elemento e. }
 }--}

quitarTodos :: (Eq t) => t-> [t]-> [t]
quitarTodos x [] = []
quitarTodos x [y]       | x == y = []
                        | otherwise = [y]
quitarTodos x (y:ys)    | x == y = quitarTodos x ys
                        | otherwise = [y] ++ quitarTodos x ys

{-- 7. eliminarRepetidos :: (Eq t) => [t]-> [t] quedeja en la lista una ´unica aparici´on de cada elemento, eliminando
 las repeticiones adicionales--}

esRepetido :: (Eq t) => t -> [t]-> [t]
esRepetido y [] = []
esRepetido y [z]    | y == z = []
                    | otherwise = [y]
esRepetido y (z:zs) | y == z = []     -- descarto y , sigo evaluando el resto de la lista
                    | otherwise = esRepetido y zs   -- 

eliminarRepetidos :: (Eq t) => [t]-> [t]
eliminarRepetidos [] = []
eliminarRepetidos [x] = [x]
eliminarRepetidos (x:xs) = esRepetido x xs ++ eliminarRepetidos xs

{--  8. mismosElementos :: (Eq t) => [t]-> [t]-> Bool, que dadas dos listas devuelve verdadero s´ı y solamente s´ı
 ambas listas contienen los mismos elementos, sin tener en cuenta repeticiones, es decir:
  problema mismosElementos (s: seq⟨T⟩, r: seq⟨T⟩) : B {
 requiere: { True }
 asegura: { resultado = true ↔ todo elemento de s pertenece r y viceversa}
 }
 --}

mismosElementosAux :: (Eq t) => t -> [t] -> Bool
mismosElementosAux x [] = True
mismosElementosAux x [y]    | x == y = True
                            | otherwise = False
mismosElementosAux x (y:ys) | x == y = True
                            | otherwise = mismosElementosAux x ys

mismosElementosLista :: (Eq t) => [t] -> [t] -> Bool
mismosElementosLista [] y = True
--mismosElementosLista x [] = True -- no hace falta este caso porque unicamente es necesario el caso [] para las listas donde haces pattern matching.
-- ademas, si llamas a esta funcion 2 veces, invirtiendo los parametros en cada llamada, entonces basta con corroborar el vacio de una de ellas
mismosElementosLista (x:xs) y = mismosElementosAux x y && mismosElementosLista xs y

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos s r = mismosElementosLista s r && mismosElementosLista r s

{-- 9. capicua :: (Eq t) => [t]-> Bool seg´un la siguiente especificaci´on:
 problema capicua (s: seq⟨T⟩) : B {
 requiere: { True }
 asegura: { (resultado = true) ↔ (s = reverso(s)) }
 }
 Por ejemplo capicua [´a’,’c’, ’b’, ’b’, ’c’, ´a’] es true, capicua [´a’, ’c’, ’b’, ’d’, ´a’] es false.--}

reversa :: (Eq t) => [t] -> [t]
reversa [] = []
reversa [x] = [x]
reversa (x:xs) = reversa xs ++ [x]

capicua :: (Eq t) -> [t] -> Bool
capicua [] = True
capicua x = x == reversa x