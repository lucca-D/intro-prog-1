{--
Ejercicio 17. Implementar la funci´on esFibonacci :: Integer->Bool seg´un la siguiente especificaci´on:
 problema esFibonacci (n: Z) : B {
 requiere: { n ≥ 0 }
 asegura: { resultado = true ↔ n es alg´un valor de la secuencia de Fibonacci definida en el ejercicio 1}
 }
--}

sucesionFibonacci :: Integer -> Integer -- OK
sucesionFibonacci 0 = 0
sucesionFibonacci 1 = 1
sucesionFibonacci n = sucesionFibonacci(n-1) + sucesionFibonacci(n-2) 

esFibonacciAux :: Integer -> Integer -> Bool
esFibonacciAux n i  | n == sucesionFibonacci i = True
                    | sucesionFibonacci i > n = False
                    | otherwise = esFibonacciAux n (i+1)

esFibonacci :: Integer -> Bool 
esFibonacci n = esFibonacciAux n 0