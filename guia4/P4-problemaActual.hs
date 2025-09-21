

type Natural = Integer
esDivisible :: Natural -> Natural -> Bool
esDivisible x y | x < y = False
                | x - y == 0 = True
                | otherwise = esDivisible (x - y) y

main :: IO()
main = do
    --print(factorial 3)
    --print(fib 10)
    --print(fib2 (-1))
    --print(parteEntera 10.0)
    print(esDivisible 50 5)