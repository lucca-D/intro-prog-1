f1 :: Integer -> Integer
f1 n    | n == 0 = 1
        | otherwise = 2^n + f1 (n-1)

f2 :: Integer -> Float -> Float
f2 n q  | n == 1 = q^n
        | otherwise = q^n + f2 (n-1) q

f3 :: Integer -> Float -> Float
f3 n q  | n == 0 = 1
        | otherwise = f2 (2*n) q

f4 ::  Integer -> Float -> Float
f4 n q  | n == 0 = 1
        | n == 1 = q^(2*n) + q^n
        | otherwise = f2 (2*n) q - f2 (n-1) q