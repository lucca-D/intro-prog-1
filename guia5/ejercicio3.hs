{--  8. multiplosDeN :: Integer-> [Integer]-> [Integer] que dado un n´umero n y una lista xs, devuelve una lista
 con los elementos de xs m´ultiplos de n. --}

multiplosDeN :: Integer-> [Integer]-> [Integer]
multiplosDeN n [x]      | mod x n == 0 = [x]
                        | otherwise = []
multiplosDeN n (x:xs)   | mod x n  == 0 = x : multiplosDeN n xs
                        | otherwise = multiplosDeN n xs