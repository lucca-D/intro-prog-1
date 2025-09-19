-- Ejercicio 7.
-- a) Implementar la funci´on:
-- distanciaManhattan:: (Float, Float, Float)-> (Float, Float, Float)-> Float
-- problema distanciaManhattan (p : R×R×R,q : R×R×R) : R {
-- requiere: {True}
-- asegura: {res = SUM2 i=0 |pi −qi|}
-- }
-- Por ejemplo:
-- distanciaManhattan (2, 3, 4) (7, 3, 8) ⇝ 9
-- distanciaManhattan ((-1), 0, (-8.5)) (3.3, 4, (-4)) ⇝ 12.8
-- b) Reimplementar la funci´on teniendo en cuenta el siguiente tipo: type Punto3D = (Float, Float, Float)

type Punto3D = (Float, Float, Float)
distanciaManhattan :: Punto3D -> Punto3D -> Float
distanciaManhattan (a, b, c) (d, e, f) = sqrt((a - d)**2) + sqrt((b - e)**2) + sqrt((c - f)**2)