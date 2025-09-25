{--
 Ejercicio 19. Implementar la funci´on esSumaInicialDePrimos :: Integer->Bool seg´un la siguiente especificaci´on:
 problema esSumaInicialDePrimos (n: Z) : B {
 requiere: { n ≥ 0 }
 asegura: { resultado = true ↔ n es igual a la suma de los m primeros n´umeros primos, para alg´un m.}
 }
--}

esPrimo :: Integer -> Bool
esPrimo 0 = False
esPrimo 1 = False
esPrimo 2 = True
esPrimo 3 = True
esPrimo n = esPrimoAux n 2

esPrimoAux :: Integer -> Integer -> Bool
esPrimoAux n k      | n == k = True
                    | mod n k == 0 = False
                    | otherwise = esPrimoAux n (k+1)

sumaDePrimos :: Integer -> Integer -> Integer -> Integer -> Integer 
sumaDePrimos i p j k    | p >= i = k
                        | not (esPrimo j) = sumaDePrimos i p (j+1) k
                        | otherwise = sumaDePrimos i (p+1) (j+1) (k+j) -- 3 1 3 2 ||| 3 2 4 5 ||| 

esSumaInicialDePrimosAux :: Integer -> Integer -> Bool
esSumaInicialDePrimosAux n i    | sumaDePrimos i 0 0 0 > n = False
                                | sumaDePrimos i 0 0 0 == n = True
                                | otherwise = esSumaInicialDePrimosAux n (i+1)

esSumaInicialDePrimos :: Integer -> Bool
esSumaInicialDePrimos 0 = False
esSumaInicialDePrimos 1 = False
esSumaInicialDePrimos n = esSumaInicialDePrimosAux n 1