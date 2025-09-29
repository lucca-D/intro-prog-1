{-- Ejercicio 8. En este ejercicio vamos a trabajar con matrices.
 Vamos a representar una matriz como una secuencia de secuencias. Si m es nuestra secuencia de secuencias que representa
 una matriz: La secuencia i-´esima de m representa la i-´esima fila de la matriz, y el elemento j-´esimo dentro de la secuencia
 i-´esima representa el elemento en la fila i, columna j de la matriz.
 Por ejemplo, a la matriz identidad de R3 la podemos definir como la lista de listas: [[1,0,0],[0,1,0],[0,0,1]] en Haskell.
 Usando esta representaci´on, definir las siguientes funciones sobre matrices:--}

{--1. sumaTotal :: [[Integer]]-> Integer seg´un la siguiente especificaci´on:
 problema sumaTotal (m: seq⟨seq⟨Z⟩⟩) : Z {
 requiere: { |m| > 0 }
 requiere: { |m[0]| > 0 }
 requiere: { Todos los elementos de la secuencia m tienen la misma longitud }
 asegura: { resultado = |m|−1
 i=0
 }--}

sumatoriaLista :: [Integer] -> Integer
sumatoriaLista [] = 0
sumatoriaLista [x] = x
sumatoriaLista (x:xs) = x + sumatoriaLista xs

sumaTotalAux :: [[Integer]] -> Integer
sumaTotalAux [] = 0
sumaTotalAux (x:xs) = sumatoriaLista x + sumaTotalAux xs

sumaTotal :: [[Integer]]-> Integer
sumaTotal x = sumaTotalAux x

{-- 2. cantidadDeApariciones :: Integer-> [[Integer]]-> Integer seg´un la siguiente especificaci´on:
 problema cantidadDeApariciones (e: Z, m: seq⟨seq⟨Z⟩⟩) : Z {
 requiere: { |m| > 0 }
 requiere: { |m[0]| > 0 }
 requiere: { Todos los elementos de la secuencia m tienen la misma longitud }
 asegura: { resultado = |m|−1
 i=0
 }--}

aparicionesPorLista :: Integer -> [Integer]-> Integer
aparicionesPorLista n [] = 0
aparicionesPorLista n (x:xs)  | n == x = 1 + aparicionesPorLista n xs
                                | otherwise = aparicionesPorLista n xs

cantidadDeApariciones :: Integer-> [[Integer]]-> Integer
cantidadDeApariciones n [] = 0
cantidadDeApariciones n (x:xs) = aparicionesPorLista n x + cantidadDeApariciones n xs

{-- 3. contarPalabras :: String->[[String]]->Int seg´un la siguiente especificaci´on:
 problema contarPalabras (p: String, m: seq⟨seq⟨String⟩⟩) : Z {
 requiere: { |m| > 0 }
 requiere: { |m[0]| > 0 }
 requiere: { Todos los elementos de la secuencia m tienen la misma longitud }
 asegura: { El resultado es la cantidad de veces que p aparece exactamente igual en los elementos de m }
 }--}

contarPalabrasAux :: String->[String]->Int
contarPalabrasAux _ [] = 0
contarPalabrasAux p (x:xs)  | p == x = 1 + contarPalabrasAux p xs
                            | otherwise = contarPalabrasAux p xs

contarPalabras :: String->[[String]]->Int
contarPalabras _ [] = 0
contarPalabras p (x:xs) = contarPalabrasAux p x + contarPalabras p xs

{-- 4. cantidadDeApariciones2 :: (Eq a) => a-> [[a]]-> Integer tal que pueda usarlo para resolver los dos ejerci
cios anteriores.--}

cantidadDeAparicionesAux2 :: (Eq a) => a-> [a]-> Integer
cantidadDeAparicionesAux2 _ [] = 0
cantidadDeAparicionesAux2 x (y:ys)  | x == y = 1 + cantidadDeAparicionesAux2 x ys
                                    | otherwise = cantidadDeAparicionesAux2 x ys

cantidadDeApariciones2 :: (Eq a) => a-> [[a]]-> Integer
cantidadDeApariciones2 _ [] = 0
cantidadDeApariciones2 x (y:ys) = cantidadDeAparicionesAux2 x y + cantidadDeApariciones2 x ys

{-- 5. multiplicarPorEscalar :: Integer-> [[Integer]]-> [[Integer]] seg´un la siguiente especificaci´on:
 problema multiplicarPorEscalar (lambda: Z, m: seq⟨seq⟨Z⟩⟩) : seq⟨seq⟨Z⟩⟩ {
 requiere: { |m| > 0 }
 requiere: { |m[0]| > 0 }
 requiere: { Todos los elementos de la secuencia m tienen la misma longitud }
 asegura: { |resultado| = m }
 asegura: { Para todo 0 ≤ i < |m|, |resultado[i]| = |m[i]| }
 asegura: { Para toda posici´on v´alida i,j de m, resultado[i][j] = lambda × m[i][j]}
 }--}

multiplicarEscalarPorLista :: Integer-> [Integer]-> [Integer]
multiplicarEscalarPorLista _ [] = []
multiplicarEscalarPorLista lambda (x:xs) = (lambda * x) : (multiplicarEscalarPorLista lambda xs)

multiplicarPorEscalar :: Integer-> [[Integer]]-> [[Integer]]
multiplicarPorEscalar _ [] = []
multiplicarPorEscalar lambda (x:xs) = [multiplicarEscalarPorLista lambda x] ++ multiplicarPorEscalar lambda xs

{--6. concatenarFilas :: [[String]]->[String] seg´un la siguiente especificaci´on:
 problema concatenarFilas (m: seq⟨seq⟨String⟩⟩) : seq⟨String⟩ {
 requiere: { |m| > 0 }
 requiere: { |m[0]| > 0 }
 requiere: { Todos los elementos de la secuencia m tienen la misma longitud }
 asegura: { |resultado| = |m| }
 asegura: { Para todo 0 ≤ i < |m|, resultado[i] = concatenaci´on de todos los strings en m[i] }
 }--}

concatenarLista :: [String] -> String
concatenarLista [] = []
concatenarLista (x:xs) = x ++ concatenarLista xs

concatenarFilas :: [[String]]->[String]
concatenarFilas [] = []
concatenarFilas (x:xs) = [concatenarLista x] ++ concatenarFilas xs

{-- 7. i´esimaFila :: Integer-> [[a]]-> [a] seg´un la siguiente especificaci´on:
 problema i´esimaFila (i: Z, m: seq⟨seq⟨T⟩⟩) : seq⟨T⟩ {
 requiere: { |m| > 0 }
 requiere: { |m[0]| > 0 }
 requiere: { Todos los elementos de la secuencia m tienen la misma longitud }
 requiere: { 0 ≤ i < |m| }
 asegura: { |resultado| = |m[i]| }
 asegura: { Para todo 0 <= j < |m[i]|, resultado[j] = m[i][j] }
 }--}

iesimaFilaAux :: Integer -> [[a]] -> Integer -> [a]
iesimaFilaAux i [] j = []
iesimaFilaAux i (x:xs) j    | i == j = x
                            | otherwise = iesimaFilaAux i xs (j+1)

iesimaFila :: Integer-> [[a]]-> [a]
iesimaFila i x = iesimaFilaAux i x 0

{-- 8. iesimaColumna :: Integer-> [[a]]-> [a] seg´un la siguiente especificaci´on:
 problema i´esimaColumna (i: Z, m: seq⟨seq⟨T⟩⟩) : seq⟨T⟩ {
 requiere: { |m| > 0 }
 requiere: { |m[0]| > 0 }
 requiere: { Todos los elementos de la secuencia m tienen la misma longitud }
 requiere: { 0 ≤ i < |m[0]| }
 asegura: { |resultado| = |m| }
 asegura: { Para todo 0 <= f < |m|, resultado[f] = m[f][i] }
 }--}

iesimoValor :: Integer -> [a] -> Integer -> a
iesimoValor i (x:xs) j  | i == j = x
                        | otherwise = iesimoValor i xs (j+1)

iesimaColumna :: Integer-> [[a]] -> [a]
iesimaColumna i [] = []
iesimaColumna i (x:xs) = [iesimoValor i x 0 ]++ iesimaColumna i xs

{--9. matrizIdentidad :: Integer-> [[Integer]] seg´un la siguiente especificaci´on:
 problema matrizIdentidad (n: Z) : seq⟨seq⟨Z⟩⟩ {
 requiere: { n > 0 }
 asegura: { |resultado| = n }
 asegura: { Para todo 0 <= i < n, |resultado[i]| = n}
 asegura: { Para todo 0 <= i < n, resultado[i][i] = 1 }
 asegura: { Para todo 0 ≤ i,j < n, si i es distinto de j entonces resultado[i][j] = 0 }
 }--}

iesimaIdentidad :: Integer -> Integer->Integer->[Integer]
iesimaIdentidad n i j   | n==j = []
                        | i==j = [1] ++ iesimaIdentidad n i (j+1)
                        | otherwise = [0] ++ iesimaIdentidad n i (j+1)
                        
matrizIdentidadAux  :: Integer-> Integer->[[Integer]]
matrizIdentidadAux n i  | n == i = []
                        | otherwise = [iesimaIdentidad n i 0] ++ matrizIdentidadAux n (i+1)

matrizIdentidad :: Integer-> [[Integer]]
matrizIdentidad 0 = []
matrizIdentidad n = matrizIdentidadAux n 0

{-- 10. cantidadParesColumna :: Integer-> [[Integer]]-> Integer seg´un la siguiente especificaci´on:
 problema cantidadParesColumna (i: Z, m: seq⟨seq⟨Z⟩⟩) : Z {
 requiere: { |m| > 0 }
 requiere: { |m[0]| > 0 }
 requiere: { Todos los elementos de la secuencia m tienen la misma longitud }
 requiere: { 0 ≤ i < |m[0]| }
 asegura: { resultado = |m|−1
 j=0 1 si m[j][i] es par, 0 si no}
 }--}

seleccionarColumnaAux :: Integer-> [Integer] -> Integer -> Integer
seleccionarColumnaAux i (x:xs) j   | i == j = x
                                | otherwise = seleccionarColumnaAux i xs (j+1)

seleccionarColumna :: Integer-> [[Integer]]-> [Integer]
seleccionarColumna i [] = []
seleccionarColumna i (x:xs) = [seleccionarColumnaAux i x 0] ++ seleccionarColumna i xs

contarParesLista :: [Integer] -> Integer
contarParesLista [] = 0
contarParesLista (x:xs)     | mod x 2 == 0 = 1 + contarParesLista xs
                            | otherwise = contarParesLista xs

cantidadParesColumna :: Integer-> [[Integer]]-> Integer
cantidadParesColumna i [] = 0
cantidadParesColumna i x = contarParesLista (seleccionarColumna i x)