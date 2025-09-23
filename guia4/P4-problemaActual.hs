
iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i    | n < 10 = n
                    | otherwise = mod (div n (10^(i-1))) 10

contarDigitos :: Integer -> Integer
contarDigitos n     | n < 10 = 1
                    | otherwise = 1 + contarDigitos(div n 10)
                    
removerPrimerYUltimoDigito :: Integer -> Integer
removerPrimerYUltimoDigito n = div (n - ((iesimoDigito n (contarDigitos n))) * (10^(contarDigitos n - 1))) 10

esCapicua :: Integer -> Bool
esCapicua n     | n < 10 = True
                | n < 1000 && iesimoDigito n 1 == iesimoDigito n (contarDigitos n) = True
                | otherwise = iesimoDigito n 1 == iesimoDigito n (contarDigitos n) && esCapicua (removerPrimerYUltimoDigito n)

main :: IO()
main = do
    --print(factorial 3)
    --print(cantDigitos 123444)
    --print(iesimoDigito 45812156 1)
    --print(removerPrimerYUltimoDigito 4521389)
    --print(iesimoDigito 4521389 (contarDigitos 4521389))
    --print(10^(contarDigitos 4521389 - 1))
    print(esCapicua 5665)
    print(esCapicua 98765432100123456789)
