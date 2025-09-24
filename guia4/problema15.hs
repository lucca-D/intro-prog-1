sumatoriaInterna :: Integer -> Integer -> Float
sumatoriaInterna n 1 = fromInteger n
sumatoriaInterna n m = fromInteger n / fromInteger m + sumatoriaInterna n (m-1)

sumaRacionales :: Integer -> Integer -> Float
sumaRacionales 1 m = sumatoriaInterna 1 m
sumaRacionales n m = sumatoriaInterna n m + sumaRacionales (n-1) m