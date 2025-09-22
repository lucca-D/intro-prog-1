sumaImpares :: Integer -> Integer
sumaImpares n   | n == 1 = 1
                | otherwise = n*2 - 1 + sumaImpares(n-1)

main :: IO()
main = do
    --print(factorial 3)
    --print(fib 10)
    --print(fib2 (-1))
    print(sumaImpares 4)