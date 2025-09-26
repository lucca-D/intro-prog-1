{--
Ejercicio 1. Definir las siguientes funciones sobre listas:
--}

--1. longitud :: [t]-> Integer, que dada una lista devuelve su cantidad de elementos.

longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

{-- 2. ultimo :: [t]-> t segun la siguiente especificacion:
 problema ultimo (s: seq⟨T⟩) : T {
 requiere: { |s| > 0 }
 asegura: { resultado = s[|s| − 1] }
 }
--}

ultimo :: [t] -> t
ultimo [x] = x
ultimo [_:xs] = ultimo xs

{--  3. principio :: [t]-> [t] seg´un la siguiente especificaci´on:
 problema principio (s: seq⟨T⟩) : seq⟨T⟩ {
 requiere: { |s| > 0 }
 asegura: { resultado = subseq(s,0,|s| − 1) } -- OJO QUE ACA FUNCIONA DE FORMA EXCLUSIVA, osea vas del elemento 0 HASTA el elemento |s| - 1 (SIN INCLUIRLO, ES DECIR HASTA EL |s| - 2 incluyo)
 }
--}

principio :: [t] -> [t]
principio [x] = []
principio (x:xs) = x : principio xs

{--  4. reverso :: [t]-> [t] seg´un la siguiente especificaci´on:
 problema reverso (s: seq⟨T⟩) : seq⟨T⟩ {
 requiere: { True }
 asegura: { resultado tiene los mismos elementos que s pero en orden inverso.}
 }
-}

reverso :: [t] -> [t]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x] 