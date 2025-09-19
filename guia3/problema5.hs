-- Ejercicio 5. Implementar la funci´on todosMenores :: (Integer, Integer, Integer)-> Bool

-- problema todosMenores (t : Z×Z×Z) : Bool {
-- requiere: {True}
-- asegura: {(res = true) ↔ ((f(t0) > g(t0)) ∧(f(t1) > g(t1)) ∧ (f(t2) > g(t2)))}
-- }
-- problema f (n : Z) : Z {
-- requiere: {True}
-- asegura: {(n ≤ 7 → res = n**2)∧(n > 7 →res =2n−1)}
-- }
-- problema g (n : Z) : Z {
-- requiere: {True}
-- asegura: {Si n es un n´umero par entonces res = n/2, en caso contrario, res = 3n+1}
-- }

f5 :: Integer -> Integer
f5 n | n <= 7 = n^2                 ---- OJO: para enteros uso ^ como potencia, NO USO **
    | otherwise = 2*n - 1

g5 :: Integer -> Integer
g5 n | mod n 2 == 0 = div n 2       ---- OJO: para enteros uso div para dividir, NO USO /
    | otherwise = 3*n + 1

todosMenores :: (Integer, Integer, Integer) -> Bool
todosMenores (a, b, c)  | (f5 a > g5 a) && (f5 ba > g5 b) && (f5 c > g5 c) = True