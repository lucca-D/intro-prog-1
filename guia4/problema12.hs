raizDe2Aprox :: Integer -> Float
raizDe2Aprox 1 = 2
raizDe2Aprox n = 2 + 1.0 / raizDe2Aprox (n-1)