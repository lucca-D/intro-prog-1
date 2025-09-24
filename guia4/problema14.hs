{---
 Ejercicio 14. Especificar e implementar una funcion sumaPotencias :: Integer->Integer->Integer->Integer que
 dados tres naturales q,n,m sume todas las potencias de la forma q^(a+b) con 1 ≤ a ≤ n y 1 ≤ b ≤ m
--}

problema sumaPotencias(q,n,m:N):N {
    requiere{True}
    asegura{resultado tendrá el valor de realizar la sumatoria de los indices propuestos para dado el q}
}

-- Primero declaro la sumatoria interna
sumatoriaInterna :: Integer -> Integer -> Integer -> Integer
sumatoriaInterna q n 1 = q^(n+1)
sumatoriaInterna q n m = q^(n+m) + sumatoriaInterna q n (m-1)

-- Ahora declaro la sumatoria externa
sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q 1 m = sumatoriaInterna q 1 m
sumaPotencias q n m = sumatoriaInterna q n m + sumatoriaInterna q (n-1) m