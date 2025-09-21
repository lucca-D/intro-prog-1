{--
Ejercicio 2. Implementar una funci´on parteEntera :: Float->Integer seg´un la siguiente especificaci´on:
 problema parteEntera (x: R) : Z {
 requiere: { x ≥ 0 }
 asegura: { resultado ≤ x < resultado+1 }
 }
--}

parteEntera :: Float -> Integer
parteEntera x   | x < 1 = 0
                | otherwise = 1 + parteEntera(x - 1)