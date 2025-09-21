{--
Ejercicio 3. Especificar e implementar la funci´on esDivisible :: Integer->Integer->Bool que dados dos n´umeros
 naturales determinar si el primero es divisible por el segundo. No est´a permitido utilizar las funciones mod ni div
--}

problema esDivisible(x,y:N):Bool {
    requiere{True}
    asegura{res=True <-> mod x y = 0}
}
type Natural = Integer
esDivisible :: Natural -> Natural -> Bool
esDivisible x y | x < y = False
                | x - y == 0 = True
                | otherwise = esDivisible (x - y) y