sucesionFibonacci :: Integer -> Integer
sucesionFibonacci 0 = 0
sucesionFibonacci 1 = 1
sucesionFibonacci n = sucesionFibonacci(n-1) + sucesionFibonacci(n-2)

main :: IO()
main = do
    --print (g 2 3)
    print(sucesionFibonacci 2)
    print(sucesionFibonacci 3)
    print(sucesionFibonacci 4)
    print(sucesionFibonacci 5)
    print(sucesionFibonacci 6)
    print(sucesionFibonacci 7)
    print(sucesionFibonacci 8)