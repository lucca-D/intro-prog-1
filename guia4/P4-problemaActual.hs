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

main :: IO()
main = do
    --print (g 2 3)
    --print(cantidadDeDigitos 123456789)
    print(esPar (iesimoDigito 123456789 1))
    print(mayorDigitoPar 1)