-- aca va el codigo que quiero probar, voy dandole :reload una vez cargado y listo lo corro
-- correr pw con admin y luego
-- ghci
-- :load "C:\\Scripts\\scripts_facultad\\intro-prog-1\\guia3\\problemaActual.hs"
-- en caso de cambios :reload
absoluto:: Integer -> Integer
absoluto x  | x >= 0 = x
            | x < 0 = -x -- tambien podia usar otherwise = -x
maximoAbsoluto :: Integer -> Integer -> Integer
maximoAbsoluto x y  | (absoluto x) > (absoluto y) = absoluto x
                        | otherwise = absoluto y
maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 x y w   | (x >= y) && (x >= w) = x
                | (y >= x) && (y >= w) = y
                | otherwise = w
main:: IO()
main = do
    print(absoluto 5)
    print(absoluto (-5))
    print(maximoAbsoluto 5 (-6))
    print(maximo3 3 6 9)