cantidadDeDigitos :: Integer -> Integer
cantidadDeDigitos n     | n<10 = 1
                        | otherwise = 1 + cantidadDeDigitos (div n 10)

iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i    | i == 1 = mod n 10
                    | otherwise = mod (div n (10^(i-1))) 10

esPar :: Integer -> Bool
esPar n     | mod n 2 == 0 = True
            | otherwise = False

buscarDigitoMasGrande :: Integer -> Integer -> Integer  -> Bool
buscarDigitoMasGrande n i j | not (esPar (iesimoDigito n j)) = False
                            | i > cantidadDeDigitos n = False
                            | iesimoDigito n j > iesimoDigito n (i) = buscarDigitoMasGrande n j (i+1)
                            | otherwise = True -- devuelve true si el digito con indice j es el par mas grande de n

mayorDigitoParAux :: Integer -> Integer -> Integer
mayorDigitoParAux n  k     | k > cantidadDeDigitos n = -1
                        | buscarDigitoMasGrande n 1 k = iesimoDigito n k -- si buscarDigitoMasGrande n 1 k es True, entonces el resultado de mayorDigitoParAux es el digito k de n
                        | otherwise = mayorDigitoParAux n (k+1)

mayorDigitoPar :: Integer -> Integer
mayorDigitoPar n = mayorDigitoParAux n 1






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

main :: IO()
main = do
    --print (g 2 3)
    --print(cantidadDeDigitos 123456789)
    --print(sumaDePrimos 1 0 0 0) --5
    --print(sumaDePrimos 2 0 0 0) --5
    --print(sumaDePrimos 3 0 0 0) --10
    --print(sumaDePrimos 4 0 0 0) --17
    --print(sumaDePrimos 5 0 0 0) --28
    --print(sumaDePrimos 6 0 0 0) --41
    print(esSumaInicialDePrimos 6)
    print(esSumaInicialDePrimos 7)
    print(esSumaInicialDePrimos 11)
    print(esSumaInicialDePrimos 13)
    print(esSumaInicialDePrimos 17)
    print(esSumaInicialDePrimos 41)
    print(esSumaInicialDePrimos 58)
