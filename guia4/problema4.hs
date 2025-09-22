{--
Ejercicio 4. Especificar e implementar la funci´on sumaImpares :: Integer->Integer que dado n ∈ N sume los primeros
 n n´umeros impares. Por ejemplo: sumaImpares 3 
1+3+5 ⇝ 9
--}
problema sumaImpares(n:N):N {
    requiere{True}
    asegura{resultado será la sumatoria de los primeros n números impares naturales}
}

sumaImpares :: Integer -> Integer
sumaImpares n   | n == 1 = 1
                | otherwise = n*2 - 1 + sumaImpares(n-1)