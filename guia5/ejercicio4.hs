{-- Ejercicio 4. a) Definir las siguientes funciones sobre listas de caracteres, interpretando una palabra como una secuencia de
 caracteres sin blancos:--}

{--a) sacarBlancosRepetidos :: [Char]-> [Char], que reemplaza cada subsecuencia de blancos contiguos de la pri
mera lista por un solo blanco en la lista resultado.--}

sacarBlancosRepetidos :: [Char]-> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos (x:xs)    | x == head xs && x == " " = sacarBlancosRepetidos xs
                                | otherwise = x : sacarBlancosRepetidos xs

{--b) contarPalabras :: [Char]-> Integer, que dada una lista de caracteres devuelve la cantidad de palabras que
 tiene.--}

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

cuentoBlancos :: [Char] -> Integer
cuentoBlancos [] = 0
cuentoBlancos [x] = 0
cuentoBlancos (x:xs)    | x == ' ' = 1 + cuentoBlancos xs
                        | otherwise = cuentoBlancos xs

hayPalabras :: [Char] -> Bool
hayPalabras [] = False
hayPalabras [x]      | x == ' ' = False
                     | otherwise = True
hayPalabras (x:xs)   | trimBlancosEnd (trimBlancosStart (sacarBlancosRepetidos (x:xs))) == "" = False
                     | otherwise = True

contarPalabras :: [Char] -> Integer
contarPalabras [] = 0
contarPalabras [x]      | x == ' ' = 0
                        | otherwise = 1
contarPalabras (x:xs)   | hayPalabras (x:xs) = cuentoBlancos (trimBlancosEnd (trimBlancosStart (sacarBlancosRepetidos (x:xs)))) + 1
                        | otherwise = 0

{-- c) palabras :: [Char]-> [[Char]], que dada una lista arma una nueva lista con las palabras de la lista original--}

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

{--  d) palabraMasLarga :: [Char]-> [Char], que dada una lista de caracteres devuelve su palabra m´as larga.--}