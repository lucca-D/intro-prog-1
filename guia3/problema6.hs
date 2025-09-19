--Ejercicio 6. Usando los siguientes tipos:
-- type Anio = Integer
-- type EsBisiesto = Bool
-- Programar la funci´on bisiesto :: Anio-> EsBisiesto seg´un la siguiente especificaci´on:
-- problema bisiesto (año : Z) : Bool {
-- requiere: {True}
-- asegura: {(res = false) ↔ (año no es m´ultiplo de 4, o bien, año es m´ultiplo de 100 pero no de 400)}
-- }
-- Por ejemplo:
-- bisiesto 1901 ⇝ False
-- bisiesto 1900 ⇝ False
-- bisiesto 1904 ⇝ True
-- bisiesto 2000 ⇝ True

type Anio = Integer
type EsBisiesto = Bool

bisiesto :: Anio -> EsBisiesto
bisiesto a  | mod a 4 /= 0 || (mod a 100 == 0 && mod a 400 /= 0) = False
            | otherwise = True
            