{--
Ejercicio 11. a) aproximacion euler
--}

factorial :: Integer -> Integer
factorial n     | n == 0 || n == 1 = 1
                | otherwise = n * factorial(n-1)

eAprox :: Integer -> Float
eAprox n    | n == 1 = 2
            | otherwise = 1 / factorial(n) + eAprox (n-1)

--b) definir e como la aproximacion de los 10 primeros terminos de eAprox
e :: Float
e = eAprox 10