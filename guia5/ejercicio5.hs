{-- Ejercicio 5. Definir las siguientes funciones sobre listas--}

{-- 1. sumaAcumulada :: (Num t) => [t]-> [t] seg´un la siguiente especificaci´on:
 problema sumaAcumulada (s: seq⟨T⟩) : seq⟨T⟩ {
 requiere: {T es un tipo num´erico}
 requiere: {cada elemento de s es mayor estricto que cero}
 asegura: {|s| = |resultado| ∧ el valor en la posici´on i de resultado es i
 k=0 s[k]}
 }
 Por ejemplo sumaAcumulada [1, 2, 3, 4, 5] es [1, 3, 6, 10, 15]--}

sumaAcumuladaAux  :: (Num t) => [t]-> [t]-> [t]
sumaAcumuladaAux [] y = []
sumaAcumuladaAux x [] = []
sumaAcumuladaAux [x] (y:ys) = [x+y]
sumaAcumuladaAux (x:xs) (y:ys)  =  (x + y) : (sumaAcumuladaAux xs [x+y])

sumaAcumulada :: (Num t) => [t]-> [t]
sumaAcumulada x = sumaAcumuladaAux x [0]

{--2. descomponerEnPrimos :: [Integer]-> [[Integer]] seg´un la siguiente especificaci´on:
 problema descomponerEnPrimos (s: seq⟨Z⟩) : seq⟨seq⟨Z⟩⟩ {
 requiere: { Todos los elementos de s son mayores a 2 }
 asegura: { |resultado| = |s| }
 asegura: {todos los valores en las listas de resultado son n´umeros primos}
 asegura: {multiplicar todos los elementos en la lista en la posici´on i de resultado es igual al valor en la posici´on
 i de s}
 }
 Por ejemplo descomponerEnPrimos [2, 10, 6] es [[2], [2, 5], [2, 3]]--}

esPrimoAux :: Integer -> Integer -> Bool
esPrimoAux 1 m = True
esPrimoAux n m  | n == m = True
                | mod n m == 0 = False
                | otherwise = esPrimoAux n (m+1)

esPrimo :: Integer -> Bool
esPrimo n = esPrimoAux n 2

proximoPrimo :: Integer -> Integer
proximoPrimo n  | esPrimo (n+1) = n+1
                | otherwise = proximoPrimo (n+1)

descomponerEnPrimosAux :: Integer -> Integer -> [Integer]
descomponerEnPrimosAux x n      | esPrimo x = [x] 
                                | mod x n == 0 = [n] ++ descomponerEnPrimosAux (div x n) 2
                                | otherwise = descomponerEnPrimosAux x (proximoPrimo n)

descomponerEnPrimos :: [Integer]-> [[Integer]]
descomponerEnPrimos [x] = [descomponerEnPrimosAux x 2]
descomponerEnPrimos (x:xs) = [descomponerEnPrimosAux x 2] ++ descomponerEnPrimos xs