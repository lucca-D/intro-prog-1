--Especificar e implementar las siguientes funciones, incluyendo su signatura.

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
--                  requiere(TRUE)
--                  asegura(requiere sera el valor maximo entre los 3 numeros enteros)
--              }

maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 x y w   | (x >= y) && (x >= w) = x
                | (y >= x) && (y >= w) = y
                | otherwise = w

-- d) algunoEsCero: dados dos numeros racionales, decide si alguno es igual a 0 (resolverlo con y sin pattern matching)
--
