{--
Ejercicio 4. Especificar e implementar la funci´on sumaImpares :: Integer->Integer que dado n ∈ N sume los primeros
 n n´umeros impares. Por ejemplo: sumaImpares 3 
1+3+5 ⇝ 9
--}
problema sumaImpares(n:N):N {
    requiere{True}
    asegura{resultado será la sumatoria de los primeros n números impares naturales}
}

-- Hago uso de la funcion esDivisible del problema 3
type Natural = Integer
esDivisible :: Natural -> Natural -> Bool
esDivisible x y | x < y = False
                | x - y == 0 = True
                | otherwise = esDivisible (x - y) y


-- NO ME SALIO