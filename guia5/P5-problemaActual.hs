sacarBlancosRepetidos :: [Char]-> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos (x:xs)    | x == head xs && x == ' ' = sacarBlancosRepetidos xs
                                | otherwise = x : sacarBlancosRepetidos xs

trimBlancosStart :: [Char] -> [Char]
trimBlancosStart [] = []
trimBlancosStart [x]        | x == ' ' = []
                            | otherwise = [x]
trimBlancosStart (x:xs)     | x == ' ' = xs
                            | otherwise = (x:xs)

trimBlancosEnd :: [Char] -> [Char]
trimBlancosEnd [] = []
trimBlancosEnd [x]      | x == ' ' = []
                        | otherwise = [x]
trimBlancosEnd (x:xs)   = x : trimBlancosEnd xs

encontrarPalabrasAux :: [Char] -> [Char]
encontrarPalabrasAux [] = []
encontrarPalabrasAux [x]        | x == ' ' = []
                                | otherwise = [x]
encontrarPalabrasAux (x:xs)     | x == ' ' = []
                                | otherwise = x : encontrarPalabrasAux xs

encontrarPalabras :: [Char] -> [Char] -> [[Char]]
encontrarPalabras [] y      = []
encontrarPalabras [x] y     = [y]
encontrarPalabras (x:xs) y  | x == ' ' = [encontrarPalabrasAux y] ++ encontrarPalabras xs xs
                            | otherwise = encontrarPalabras xs y

doyFormato :: [Char] -> [Char]
doyFormato x = trimBlancosEnd (trimBlancosStart (sacarBlancosRepetidos x))

palabras :: [Char]-> [[Char]]
palabras x = encontrarPalabras (doyFormato x) (doyFormato x)

-- main -
main :: IO()
main = do
    print(palabras "hola a todos")
    print(palabras " hola  a   todos   ")
    print(palabras "     hola a  todos")
    print(palabras "hola a todos")
    print(palabras "hola a todos asd  aa a         fdd  fd")
    print(palabras "hola a todos   ")