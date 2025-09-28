{-- Ejercicio 7. En este ejercicio trabajaremos con lockers de una facultad.
 Para resolverlo usaremos un tipo MapaDelockers que ser´a una secuencia de locker.
 Cada locker es una tupla con la primera componente correspondiente al n´umero de identificaci´on, y la segunda componente
 el estado.
 El estado es a su vez una tupla cuya primera componente dice si esta ocupado (False) o libre (True), y la segunda
 componente es un texto con el c´odigo de ubicaci´on del locker.
 type Identificacion = Integer
 type Ubicacion = Texto
 type Estado = (Disponibilidad, Ubicacion)
 type Locker = (Identificacion, Estado)
 type MapaDeLockers = [Locker]
 type Disponibilidad = Bool--}

{--1. Implementar existeElLocker :: Identificacion->MapaDeLockers->Bool, una funci´on para saber si un locker
 existe en la facultad.--}

type Texto = [Char]
type Identificacion = Integer
type Disponibilidad = Bool

type Ubicacion = Texto
type Estado = (Disponibilidad, Ubicacion)
type Locker = (Identificacion, Estado)
type MapaDeLockers = [Locker]

existeElLocker :: Identificacion -> MapaDeLockers -> Bool
existeElLocker _ [] = False
existeElLocker id ((idL,est):ls)    | id == idL = True
                                    | otherwise = existeElLocker id ls

{-- 2. Implementar ubicacionDelLocker :: Identificacion->MapaDeLockers->Ubicacion, una funci´on que dado un
 locker que existe en la facultad, me dice la ubicaci´on del mismo--}

ubicacionDelLocker :: Identificacion -> MapaDeLockers -> Ubicacion
ubicacionDelLocker _ [] = "No existe el locker"
ubicacionDelLocker id ((idL,(disp,ubi)):ls)     | id == idL = ubi
                                                | otherwise = ubicacionDelLocker id ls

{-- 3. Implementar estaDisponibleElLocker :: Identificacion->MapaDeLockers->Bool, una funci´on que dado un
 locker que existe en la facultad, me devuelve Verdadero si esta libre--}

estaDisponibleElLocker :: Identificacion->MapaDeLockers->Bool
estaDisponibleElLocker _ [] = False
estaDisponibleElLocker id ((idL,(disp,ubi)):ls) | id == idL = disp
                                                | otherwise = estaDisponibleElLocker id ls

{--4. Implementar ocuparLocker :: Identificacion->MapaDeLockers->MapaDeLockers, una funci´on que dado un loc
ker que existe en la facultad, y est´a libre, lo ocupa.--}

ocuparLocker :: Identificacion->MapaDeLockers->MapaDeLockers
ocuparLocker _ [] = []
ocuparLocker id ((idL,(disp,ubi)):ls)     | id == idL && estaDisponibleElLocker id ((idL,(disp,ubi)):ls) = [(idL,(False,ubi))] ++ ocuparLocker id ls
                                        | otherwise = [(idL,(disp,ubi))] ++ ocuparLocker id ls