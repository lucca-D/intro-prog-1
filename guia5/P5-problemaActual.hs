maximoAux :: Integer -> [Integer] -> Integer
maximoAux x [] = x
maximoAux x [y]     | x >= y = x
                    | otherwise = y
maximoAux x (y:ys)  | x >= y = maximoAux x ys
                    | otherwise = maximoAux y ys

maximo :: [Integer]-> Integer
maximo [] = 0
maximo [x] = x
maximo (x:xs) = maximoAux x xs

quitar :: Integer -> [Integer] -> [Integer]
quitar n [] = []
quitar n [x]        | n == x = []
                    | otherwise = [x]
quitar n (x:xs)     | n == x = xs
                    | otherwise = x : quitar n xs

ordenar :: [Integer]-> [Integer]
ordenar [] = []
ordenar [x] = [x]
ordenar (x:xs) = ordenar (quitar (maximo (x:xs)) (x:xs)) ++ [maximo (x:xs)]

-- main -
main :: IO()
main = do
    print(ordenar [4,3,2,1,0,10,15,68])
    print(ordenar [1,2,3])