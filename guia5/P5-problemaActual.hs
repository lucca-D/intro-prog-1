principio :: [t] -> [t]
principio [x] = []
principio (x:xs) = x : principio xs

reverso :: [t] -> [t]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x] 

-- main
main :: IO()
main = do
    print(principio [1,2,3])
    print(reverso [1,2,3])
