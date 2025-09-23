{--
 Ejercicio 9. Especificar e implementar una funci´on esCapicua :: Integer->Bool que dado n ∈ N≥0 determina si n es un n´umero capic´ua
--}

{--
 Como lo pienso? Bueno, arranco comparando los "extremos" de un numero, y luego voy para adentro, siempre comparando los "extremos".
 Entonces puedo desglosar el problema en problemas mas pequeños:
 i) Necesito una funcion para "seleccionar" digitos
 ii) Necesito una funcion para contar digitos
 iii) Necesito una funcion para "remover" el primer y el ultimo digito. Esta función será necesaria para la recursividad de esCapicua, ya que esCapicua deberá ir iterando "removiendo" cada "extremo" del numero para ir comparandolos
--}

contarDigitos :: Integer -> Integer
contarDigitos n     | n < 10 = 1
                    | otherwise = 1 + contarDigitos(div n 10)

iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i    | n < 10 = n
                    | mod (div n 10^(i-1)) 10

removerPrimerYUltimoDigito :: Integer -> Integer
removerPrimerYUltimoDigito n = div (n - ((iesimoDigito n (contarDigitos n))) * (10^(contarDigitos n - 1))) 10

esCapicua :: Integer -> Bool
esCapicua n     | n < 10 = True
                | n < 1000 && iesimoDigito n 1 == iesimoDigito n (contarDigitos n) = True
                | iesimoDigito n 1 == iesimoDigito n (contarDigitos n) && esCapicua (removerPrimerYUltimoDigito n)
                | otherwise = False