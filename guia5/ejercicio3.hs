{-- Ejercicio 3. Definir las siguientes funciones sobre listas de enteros:--}

{-- 1. sumatoria :: [Integer]-> Integer seg´un la siguiente especificaci´on:
 problema sumatoria (s: seq⟨Z⟩) : Z {
 requiere: { True }
 asegura: { resultado = |s|−1
 i=0 s[i] }
 }--}

sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria [x] = x
sumatoria (x:xs) = x + sumatoria xs


{-- 2. productoria :: [Integer]-> Integer seg´un la siguiente especificaci´on:
 problema productoria (s: seq⟨Z⟩) : Z {
 requiere: { True }
 asegura: { resultado = |s|−1
 i=0 s[i] }
 }--}

productoria :: [Integer] -> Integer
productoria [] = 0
productoria [x] = x
productoria (x:xs) = x * productoria xs


{--  3. maximo :: [Integer]-> Integer seg´un la siguiente especificaci´on:
 problema maximo (s: seq⟨Z⟩) : Z {
 requiere: { |s| > 0 }
 asegura: { resultado ∈ s ∧ todo elemento de s es menor o igual a resultado }
 }--}

maximoAux :: Integer -> [Integer] -> Integer
maximoAux x [] = x
maximoAux x [y]     | x >= y = x
                    | otherwise = y
maximoAux x (y:ys)  | x >= y = maximoAux x ys
                    | otherwise = y -- MAL: tendria que ser otherwise = maximoAux y ys

maximo :: [Integer]-> Integer
maximo [] = 0
maximo [x] = x
maximo (x:xs) = maximoAux xs

{-- 4. sumarN :: Integer-> [Integer]-> [Integer] seg´un la siguiente especificaci´on:
 problema sumarN (n: Z, s: seq⟨Z⟩) : seq⟨Z⟩ {
 requiere: { True }
 asegura: {|resultado| = |s| ∧ cada posici´on de resultado contiene el valor que hay en esa posici´on en s sumado
 n }
 }--}

sumarN :: Integer-> [Integer]-> [Integer]
sumarN _ [] = []
sumarN n [x] = [x+n]
sumarN n (x:xs) = (n+x) : sumarN n xs

{--5. sumarElPrimero :: [Integer]-> [Integer] seg´un la siguiente especificaci´on:
 problema sumarElPrimero (s: seq⟨Z⟩) : seq⟨Z⟩ {
 requiere: { |s| > 0 }
 asegura: {resultado = sumarN(s[0],s) }
 }--}

sumarElPrimero :: [Integer]-> [Integer]
sumarElPrimero [] = []
sumarElPrimero [x] = sumarN x [x]
sumarElPrimero (x:xs) = sumarN x (x:xs)

{-- 6. sumarElUltimo :: [Integer]-> [Integer] seg´un la siguiente especificaci´on:
 problema sumarElUltimo (s: seq⟨Z⟩) : seq⟨Z⟩ {
 requiere: { |s| > 0 }
 asegura: {resultado = sumarN(s[|s| − 1],s) }
 }--}

ultimoLista :: [Integer] -> Integer
ultimoLista [] = 0
ultimoLista [x] = x
ultimoLista (x:xs) = ultimoLista xs

sumarElUltimo :: [Integer]-> [Integer]
sumarElUltimo [] = []
sumarElUltimo [x] = sumarN x [x]
sumarElUltimo (x:xs) = sumarN (ultimoLista (x:xs)) (x:xs)

{-- 7. pares :: [Integer]-> [Integer] seg´un la siguiente especificaci´on:
 problema pares (s: seq⟨Z⟩) : seq⟨Z⟩ {
 requiere: { True }
 asegura: {resultado s´olo tiene los elementos pares de s en el orden dado, respetando las repeticiones}
 }--}

pares :: [Integer]-> [Integer]
pares [] = []
pares [x]       | mod x 2 == 0 = [x]
                | otherwise = []
pares (x:xs)    | mod x 2 == 0 = x : pares xs
                | otherwise = pares xs

{--  8. multiplosDeN :: Integer-> [Integer]-> [Integer] que dado un n´umero n y una lista xs, devuelve una lista
 con los elementos de xs m´ultiplos de n. --}

multiplosDeN :: Integer-> [Integer]-> [Integer]
multiplosDeN n [x]      | mod x n == 0 = [x]
                        | otherwise = []
multiplosDeN n (x:xs)   | mod x n  == 0 = x : multiplosDeN n xs
                        | otherwise = multiplosDeN n xs

{-- 9. ordenar :: [Integer]-> [Integer] que ordena los elementos de la lista en forma creciente. Sugerencia: Pensar
 c´omo pueden usar la funci´on m´aximo para que ayude a generar la lista ordenada necesaria.--}

quitar :: Integer -> [Integer] -> [Integer]
quitar n [] = []
quitar n [x]        | n == x = []
                    | otherwise = [x]
quitar n (x:xs)     | n == x = xs
                    | otherwise = x : quitar n xs

ordenar :: [Integer]-> [Integer]
ordenar [] = []
ordenar [x] = [x]
ordenar (x:xs) = ordenar (quitar (maximo (x:xs)) (x:xs)) ++ [maximo (x:xs)]