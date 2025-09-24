{--
f(n,m) que calcule la sumatoria desde i=0 hasta n de la sumatoria desde j=0 hasta m del parámetro i^j, con n m enteros
--}

-- Defino la sumatoria interna 
g :: Integer -> Integer -> Integer
g i 1 = i
g i m = i^m + g i (m-1)

-- Defino la sumatoria externa
f :: Integer -> Integer -> Integer
f 1 m = g 1 m
f n m = g n m + f (n-1) m