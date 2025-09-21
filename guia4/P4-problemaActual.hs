factorial n
 | n == 0 = 1
 | n > 0 = n * factorial (n-1)

fib :: Integer -> Integer
fib n   | n == 0 = 0
        | n == 1 = 1
        | otherwise = fib (n-1) + fib (n-2) 

fib2 :: Integer -> Integer
fib2 0 = 0
fib2 1 = 1
fib2 n = fib2 (n - 1) + fib2 (n - 2)

main :: IO()
main = do
    print(factorial 3)
    print(fib 10)
    print(fib2 (-1))