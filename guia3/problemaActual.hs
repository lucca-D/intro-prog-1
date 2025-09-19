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

estanRelacionados :: Integer -> Integer -> Bool
estanRelacionados a b   | (a /= 0) && (b /= 0) && (mod a b == 0) = True
                        | otherwise = False -- esto quizas ni es necesario porque no está en el 'asegura'

type Punto2D = (Float,Float)

productoInterno :: Punto2D -> Punto2D -> Float
productoInterno (a, b) (c, d) = a * c + b * d
productoInterno2 :: ((Float, Float), (Float, Float) )-> Float
productoInterno2 ((a, b),(c, d) )= a * c + b * d

esParMenor :: (Float, Float) -> (Float, Float) -> Bool
esParMenor (a,b) (c,d)  | (a>c) && (b>d) == True = True
                        | otherwise = False

distancia :: (Float, Float) ->  (Float, Float) ->  Float
distancia (a,b) (c,d) = sqrt((a-c)**2 + (b-d)**2)

sumarSoloMultiplos :: (Integer, Integer, Integer) -> Integer -> Integer
sumarSoloMultiplos (a,b,c) k | mod a k == 0 && mod b k == 0 && mod c k == 0 = a+b+c
                             | mod a k == 0 && mod b k == 0 = a+b
                             | mod a k == 0 &&  mod c k == 0 = a+c
                             | mod b k == 0 && mod c k == 0 = b+c
                             | mod a k == 0 = a
                             | mod b k == 0 = b
                             | mod c k == 0 = c
                             | otherwise = 0

crearPar :: t1 -> t2 -> (t1,t2)
crearPar t1 t2 = (t1,t2)

f5 :: Integer -> Integer
f5 n | n <= 7 = n^2
    | otherwise = 2*n - 1

g5 :: Integer -> Integer
g5 n | mod n 2 == 0 = div n 2
    | otherwise = 3*n + 1

main:: IO()
main = do
    --print(absoluto (-5))
    --print(maximoAbsoluto 5 (-6))
    --print(maximo3 3 6 9)
    --print(algunoEsCero 5.5 0)
    --print(algunoEsCero2 5.5 1.1)
    --print(ambosSonCero 0 1)
    --print(ambosSonCero2 1 0)
    --print(sumaDistintos 2 2 2)
    --print(esMultiploDe 10 2)
    --print(estanRelacionados 0 5)
    --print(mod 0 5)
    --print(productoInterno (2.5,7.7) (144.3,534.75654))
    --print(productoInterno2 ((2.5,7.7), (144.3,534.75654)))
    --print(esParMenor (5.5,6.6) (4.4,8.5))
    --print(distancia (1.0,1.0) (2.0,1.0))
    --print(sumarSoloMultiplos (4,4,3) 2)
    --print(crearPar 5 True)
    print(f5 7)
    print(f5 8)
    print(g5 4)
    print(g5 5)