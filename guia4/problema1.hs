{--
Ejercicio 1. Implementar la funci´on fibonacci:: Integer->Integer que devuelve el i-´esimo n´umero de Fibonacci. Recordar que la secuencia de Fibonacci se define como:
 
 fib(n) =   | 0 si n=0
            | 1 si n=1
            | fib(n-1) + fib(n-2) otro caso
 
 problema fibonacci (n: Z) : Z {
 requiere: { n ≥ 0 }
 asegura: { resultado = fib(n) }
 }
--}

fib :: Integer -> Integer
fib n   | n == 0 = 0
        | n == 1 = 1
        | otherwise = fib (n-1) + fib (n-2) 

fib :: Integer -> Integer
fib n   | n == 0 = 0
        | n == 1 = 1
        | otherwise = fib (n-1) + fib (n-2) 

fib2 :: Integer -> Integer
fib2 0 = 0
fib2 1 = 1
fib2 n = fib2 (n - 1) + fib2 (n - 2)