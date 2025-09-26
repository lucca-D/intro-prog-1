esRepetido :: (Eq t) => t -> [t]-> [t]
esRepetido y [z]    | y == z = []
                    | otherwise = [z]
esRepetido y (z:zs) | y == z = zs
                    | otherwise = z : esRepetido y zs

eliminarRepetidos :: (Eq t) => [t]-> [t]
eliminarRepetidos [] = []
eliminarRepetidos [x] = [x]
eliminarRepetidos (x:xs) = eliminarRepetidos (esRepetido x xs)

-- main
main :: IO()
main = do
    print(eliminarRepetidos [1,2,3,1])
