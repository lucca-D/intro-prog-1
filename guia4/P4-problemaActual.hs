medioFact :: Integer -> Integer
medioFact n | n == 0 = 1
            | n == 1 = 1
            | n-2 >= 0 = n * medioFact (n-2)

main :: IO()
main = do
    --print(factorial 3)
    --print(fib 10)
    --print(fib2 (-1))
    --print(parteEntera 10.0)
    print(medioFact 100000)