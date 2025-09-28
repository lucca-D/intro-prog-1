type Texto = [Char]
type Identificacion = Integer
type Disponibilidad = Bool

type Ubicacion = Texto
type Estado = (Disponibilidad, Ubicacion)
type Locker = (Identificacion, Estado)
type MapaDeLockers = [Locker]

existeElLocker :: Identificacion -> MapaDeLockers -> Bool
existeElLocker _ [] = False
existeElLocker id ((idL,e):ls)      | id == idL = True
                                    | otherwise = existeElLocker id ls

ubicacionDelLocker :: Identificacion -> MapaDeLockers -> Ubicacion
ubicacionDelLocker _ [] = "No existe el locker"
ubicacionDelLocker id ((idL,(disp,ubi)):ls)     | id == idL = ubi
                                                | otherwise = ubicacionDelLocker id ls

estaDisponibleElLocker :: Identificacion->MapaDeLockers->Bool
estaDisponibleElLocker _ [] = False
estaDisponibleElLocker id ((idL,(disp,ubi)):ls) | id == idL = disp
                                                | otherwise = estaDisponibleElLocker id ls

ocuparLocker :: Identificacion->MapaDeLockers->MapaDeLockers
ocuparLocker _ [] = []
ocuparLocker id ((idL,(disp,ubi)):ls)     | id == idL && estaDisponibleElLocker id ((idL,(disp,ubi)):ls) = [(idL,(False,ubi))] ++ ocuparLocker id ls
                                        | otherwise = [(idL,(disp,ubi))] ++ ocuparLocker id ls
-- main -
main :: IO()
main = do
    --print(existeElLocker 1 [(1,(True,"1er Piso")),(2,(False,"1er Piso")),(3,(True,"2do Piso"))])
    --print(existeElLocker 2 [(1,(True,"1er Piso")),(2,(False,"1er Piso")),(3,(True,"2do Piso"))])
    --print(existeElLocker 3 [(1,(True,"1er Piso")),(2,(False,"1er Piso")),(3,(True,"2do Piso"))])
    --print(ubicacionDelLocker 1 [(1,(True,"1er Piso")),(2,(False,"1er Piso")),(3,(True,"2do Piso"))])
    --print(ubicacionDelLocker 3 [(1,(True,"1er Piso")),(2,(False,"1er Piso")),(3,(True,"2do Piso"))])
    --print(ubicacionDelLocker 4 [(1,(True,"1er Piso")),(2,(False,"1er Piso")),(3,(True,"2do Piso"))])
    --print(estaDisponibleElLocker 1 [(1,(True,"1er Piso")),(2,(False,"1er Piso")),(3,(True,"2do Piso"))])
    --print(estaDisponibleElLocker 2 [(1,(True,"1er Piso")),(2,(False,"1er Piso")),(3,(True,"2do Piso"))])
    print(ocuparLocker 1 [(1,(True,"1er Piso")),(2,(False,"1er Piso")),(3,(True,"2do Piso"))])
    print(ocuparLocker 2 [(1,(True,"1er Piso")),(2,(False,"1er Piso")),(3,(True,"2do Piso"))])
    print(ocuparLocker 3 [(1,(True,"1er Piso")),(2,(False,"1er Piso")),(3,(True,"2do Piso"))])
    print(ocuparLocker 4 [(1,(True,"1er Piso")),(2,(False,"1er Piso")),(3,(True,"2do Piso"))])
    