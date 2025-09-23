factorial :: Integer -> Integer
factorial n     | n == 0 || n == 1 = 1
                | otherwise = n * factorial(n-1)

eAprox :: Integer -> Float
eAprox 0 = 1
eAprox n = 1.0 / fromInteger (factorial n) + eAprox (n-1)

e :: Float
e = eAprox 10

main :: IO()
main = do
    print(eAprox 8)
    print(eAprox 9)
    print(eAprox 10)
    print(e)