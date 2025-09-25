sumatoria  :: [Int] -> Int
sumatoria x     | x == [] = 0
                | otherwise = head x + sumatoria (tail x)

sumatoriaPM :: [Int] -> Int
sumatoriaPM [] = 0
sumatoriaPM (x:xs) = x + sumatoriaPM xs

pertenece :: Int -> [Int] -> Bool
pertenece x s   | s == [] = False
                | x == head s = True
                | otherwise = pertenece x (tail s)



-- main
main :: IO()
main = do
    print(sumatoriaPM [1,2])
    print(sumatoria [1,2])
    print(pertenece 3 [1,3])
