iesimoElementoFila :: Integer-> [Integer] -> Integer -> Integer
iesimoElementoFila i (x:xs) j   | i == j = x
                                | otherwise = iesimoElementoFila i xs (j+1)

seleccionarColumna :: Integer-> [[Integer]]-> [Integer]
seleccionarColumna i [] = []
seleccionarColumna i (x:xs) = [iesimoElementoFila i x 0] ++ seleccionarColumna i xs

contarParesLista :: [Integer] -> Integer
contarParesLista [] = 0
contarParesLista (x:xs)     | mod x 2 == 0 = 1 + contarParesLista xs
                            | otherwise = contarParesLista xs

cantidadParesColumna :: Integer-> [[Integer]]-> Integer
cantidadParesColumna i [] = 0
cantidadParesColumna i x = contarParesLista (seleccionarColumna i x)

-- main -
main :: IO()
main = do
    print(iesimoElementoFila 0 [1,2,3] 0)
    print(iesimoElementoFila 1 [1,2,3] 0)
    print(iesimoElementoFila 2 [1,2,3] 0)
    print(cantidadParesColumna 0 [[1,2,3],[1,2,3],[1,2,3]])
    print(cantidadParesColumna 1 [[1,2,3],[1,2,3],[1,2,3]])
    print(cantidadParesColumna 2 [[1,2,3],[1,2,3],[1,2,3]])
    