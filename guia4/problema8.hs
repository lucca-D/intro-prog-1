{--
Ejercicio 8. Especificar e implementar la funci´on sumaDigitos :: Integer->Integer que calcula la suma de d´ıgitos de un n´umero natural. Para esta funci´on pueden utilizar div y mod.
--}

problema sumaDigitos(n:N):N{
    requiere{True}
    asegura{resultado sera la suma de los digitos de n}
}

sumaDigitos :: Integer -> Integer
sumaDigitos n   | n < 10 = n
                | otherwise = mod n 10 + sumaDigitos(div n 10)