g :: Integer -> Integer -> Integer --
g i 1 = i
g i m = i^m + g i (m-1)

f :: Integer -> Integer -> Integer
f 1 m = g 1 m
f n m = g n m + f (n-1) m

main :: IO()
main = do
    --print (g 2 3)
    print(f 2 2)
    print(g 2 1)
    print(g 2 2)