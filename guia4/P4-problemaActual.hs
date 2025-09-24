raizDe2Aprox :: Integer -> Float
raizDe2Aprox 1 = 2
raizDe2Aprox n = 2 + 1.0 / raizDe2Aprox (n-1)

main :: IO()
main = do
    print(raizDe2Aprox 2)
    print(raizDe2Aprox 3)
    print(raizDe2Aprox 4000)
