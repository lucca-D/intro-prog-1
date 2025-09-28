esRepetido :: (Eq t) => t -> [t]-> [t]
esRepetido y [] = []
esRepetido y [z]    | y == z = []
                    | otherwise = [y]
esRepetido y (z:zs) | y == z = []     -- descarto y , sigo evaluando el resto de la lista
                    | otherwise = esRepetido y zs   -- 

eliminarRepetidos :: (Eq t) => [t]-> [t]
eliminarRepetidos [] = []
eliminarRepetidos [x] = [x]
eliminarRepetidos (x:xs) = esRepetido x xs ++ eliminarRepetidos xs

-- main -
main :: IO()
main = do
    print(eliminarRepetidos [1,2,3])
    print(eliminarRepetidos [1,2,3,1])
