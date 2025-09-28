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

-- main -
main :: IO()
main = do
    print(descomponerEnPrimosAux 4 2)
    print(descomponerEnPrimosAux 5 2)
    print(descomponerEnPrimosAux 6 2)
    print(descomponerEnPrimosAux 7 2)
    print(descomponerEnPrimosAux 8 2)
    print(descomponerEnPrimosAux 9 2)
    print(descomponerEnPrimosAux 10 2)
    print(descomponerEnPrimosAux 11 2)
    print(descomponerEnPrimosAux 21 2)
    print(descomponerEnPrimosAux 33 2)
    --
    print(descomponerEnPrimosAux 128 2)
    --
    print(descomponerEnPrimos [4,5,6,7,8,9,10,11,21,33])
    