reversa :: (Eq t) => [t] -> [t]
reversa [] = []
reversa [x] = [x]
reversa (x:xs) = reversa xs ++ [x]

capicua :: (Eq t) => [t] -> Bool
capicua [] = True
capicua x = x == reversa x

-- main -
main :: IO()
main = do
    print(reversa [4,3,2,1])
    print(capicua [1,2,3])
    print(capicua [3,2,3])