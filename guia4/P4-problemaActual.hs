sumaDigitos :: Integer -> Integer
sumaDigitos n   | n < 10 = n
                | otherwise = mod n 10 + sumaDigitos(div n 10)

main :: IO()
main = do
    --print(factorial 3)
    print(sumaDigitos 123)
