{--
Ejercicio 6. Implementar la funci´on todosDigitosIguales :: Integer->Bool que determina si todos los d´ıgitos de un n´umero natural son iguales, es decir:
 problema todosDigitosIguales (n: Z) : B {
 requiere: { n > 0 }
 asegura: { resultado = true ↔ todos los d´ıgitos de n son iguales }
 }
--}

todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n   | n < 10 = True
                        | div n 10 == 0 =?
                        | mod n 10 == div n 10 == div n 100 == div n 1000