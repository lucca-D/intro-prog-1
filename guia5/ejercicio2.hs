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
esRepetido y [z] = y == z = []
esRepetido y (z:zs) | y == z = zs
                    | otherwise = z : esRepetido y zs

eliminarRepetidos :: (Eq t) => [t]-> [t]
eliminarRepetidos [] = []
eliminarRepetidos [x] = [x]
eliminarRepetidos (x:xs) = esRepetido x xs ++ eliminarRepetidos xs