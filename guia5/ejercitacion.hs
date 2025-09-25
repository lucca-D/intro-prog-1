{--
1. longitud::[Int]→Int  queindicacu´antoselementostieneunalista.
 2. sumatoria::[Int]→Int  queindicalasumadeloselementosdeunalista.
 3. pertenece::Int→[Int]→Bool  queindicasiunelementoapareceenlalista.Porejemplo:
 pertenece9[]⇝False
 pertenece9[1,2,3]⇝False
 pertenece9[1,2,9,9,−1,0]⇝Tru
--}

longitud :: [Int] -> Int
longitud x | x == []   = 0
           | otherwise = 1 + longitud (tail x)

longitudPM :: [Int] -> Int
longitudPM [] = 0
longitudPM (_:xs) = 1 + longitudPM xs   --longitud (x:xs) = 1 + longitud xs es lo mismo, en pattern matching esta es una forma de desestructurar la lista y "definir" xs
                                        -- no hay diferencia, solo en la lectura supuestamente es mas "sencillo" con _
sumatoria  :: [Int] -> Int
sumatoria x     | x == [] = 0
                | otherwise = head x + sumatoria (tail x)

sumatoriaPM :: [Int] -> Int
sumatoriaPM [] = 0
sumatoriaPM (x:xs) = x + sumatoriaPM xs

pertenece :: Int -> [Int]-> Bool
pertenece x s   | s == [] = False
                | x == head s = True
                | otherwise = pertenece x (tail s)

pertenece :: Int -> [Int]-> Bool
pertenece _ [] = False
pertenece x (y:ys)  | x == y = True
                    | otherwise = pertenece x ys