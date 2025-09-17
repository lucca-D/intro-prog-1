-- aca va el codigo que quiero probar, voy dandole :reload una vez cargado y listo lo corro
-- correr pw con admin y luego
-- ghci
-- :load "C:\\Scripts\\scripts_facultad\\intro-prog-1\\guia3\\problemaActual.hs"
-- :load "D:\\facultad\\gitRepo\\intro-prog-1\\guia3\\problemaActual.hs"
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
algunoEsCero :: Float -> Float -> Bool
algunoEsCero x y    | (x == 0) || (y == 0) = True
                    | otherwise = False
algunoEsCero2 :: Float -> Float -> Bool
algunoEsCero2 0 _ = True
algunoEsCero2 _ 0 = True
algunoEsCero2 _ _ = False

ambosSonCero :: Float -> Float -> Bool
ambosSonCero x y    | (x == y) && (x == 0 )= True
                    | otherwise = False
ambosSonCero2 :: Float -> Float -> Bool
ambosSonCero2 0 0 = True
ambosSonCero2 _ _ = False

sumaDistintos :: Integer -> Integer -> Integer -> Integer
sumaDistintos x y z | x == y && y == z = 0
                    | x /= y && y /= z && x /= z = x+y+z
                    | x==y = z
                    | x==z = y
                    | z==y = x

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y    | (x>0) && (y>0) && (mod x y == 0) = True
                    | otherwise = False

main:: IO()
main = do
    --print(absoluto (-5))
    --print(maximoAbsoluto 5 (-6))
    --print(maximo3 3 6 9)
    --print(algunoEsCero 5.5 0)
    --print(algunoEsCero2 5.5 1.1)
    print(ambosSonCero 0 1)
    print(ambosSonCero2 1 0)
    print(sumaDistintos 2 2 2)
    print(esMultiploDe 10 2)