--2) Especificar e implementar las siguientes funciones, incluyendo su signatura.

--a) absoluto: calcula el valor absoluto de un numero entero.
-- problema absoluto(x:Z):Z {
--              requiere{TRUE}
--              asegura{resultado sera el valor absoluto de x}
--          }

absoluto:: Integer -> Integer
absoluto x  | x >= 0 = x
            | x < 0 = -x -- tambien podia usar otherwise = -x

--b) maximoAbsoluto: devuelve el maximo entre el valor absoluto de dos numeros enteros.
-- problema maximoAbsoluto(x:Z,y:Z):Z {
--              requiere{TRUE}
--              asegura{resultado sera el maximo entre el valor absoluto de dos numeros enteros}
--          }
maximoAbsoluto :: Integer -> Integer -> Integer
maximoAbsoluto x y  | (absoluto x) > (absoluto y) = absoluto x
                    | otherwise = absoluto y

-- c) maximo3: devuelve el maximo entre tres numeros enteros
-- problema     maximo3(x:Z,y:Z,w:Z):Z {
--                  requiere{TRUE}
--                  asegura{requiere sera el valor maximo entre los 3 numeros enteros}
--       mejor asi: asegura{(res = x) V (res = y) V (res = w)}
--                  asegura{(res ≥ x) ∧ (res ≥ y) ∧ (res ≥ z)}
--              }

maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 x y w   | (x >= y) && (x >= w) = x
                | (y >= x) && (y >= w) = y
                | otherwise = w

-- d) algunoEsCero: dados dos numeros racionales, decide si alguno es igual a 0 (resolverlo con y sin pattern matching)
-- problema algunoEsCero(x : Q, y: Q) : Boolean {
--              requiere{TRUE}
--              asegura{res sera True si x=0 o y=0}
--              asegura{res sera False si (x /= 0) y (y/=0)}
--}

algunoEsCero :: Float -> Float -> Bool
algunoEsCero x y    | (x == 0) || (y == 0) = True
                    | otherwise = False
-- con pattern matching:
algunoEsCero2 :: Float -> Float -> Bool
algunoEsCero2 0 _ = True
algunoEsCero2 _ 0 = True
algunoEsCero2 _ _ = False -- va evaluando paso a paso; si el 1er argumento no es 0 entonces va al 2do, si tampoco es entonces llega al 3ro, el cual le dice que resultaria False

-- e) ambosSonCero: dados dos numeros racionales, decide si ambos son iguales a 0 (resolverlo con y sin pattern matching).
--  problema ambosSonCero(x:Q , y:Q) : Bool {
--      requiere{TRUE}
--      asegura{req sera True si x = y = 0}
--      asegura{req sera False si x /= 0 o y /= 0}
--}

ambosSonCero :: Float -> Float -> Bool
ambosSonCero x y    | (x == y) && (x == 0 )= True
                    | otherwise = False
ambosSonCero2 :: Float -> Float -> Bool
ambosSonCero2 0 0 = True
ambosSonCero2 _ _ = False

-- g) sumaDistintos: que dados tres numeros enteros calcule la suma sin sumar repetidos (si los hubiera)
--  problema sumaDistintos(x:Z,y:Z,w:Z) : Z {
--      requiere{True}
--      asegura{res sera 0 si los 3 parametros son iguales}
--      asegura{si 2 parámetros son iguales, res será igual al distinto}
--      asegura{si todos los parámetros son distintos, res sera x+y+w}
--  }

sumaDistintos :: Integer -> Integer -> Integer -> Integer
sumaDistintos x y z | x == y && y == z = 0
                    | x /= y && y /= z && x /= z = x+y+z
                    | x==y = z
                    | x==z = y
                    | z==y = x

-- h) esMultiploDe: dados dos numeros naturales, decide si el primero es multiplo del segundo
-- problema esMultiploDe (x:Z,y:Z): Bool {
--      requiere{los dos parámetros deben ser mayor a 0}
--      asegura{res es True si un parámetro es múltiplo del otro}
--      asegura{res es False si los parámetros no son múltiplos}
--}

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y    | (x>0) && (y>0) && (mod x y == 0) = True
                    | otherwise = False

