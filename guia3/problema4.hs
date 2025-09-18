-- Ejercicio 4. ⋆ Especificar e implementar las siguientes funciones utilizando tuplas para representar pares y ternas de
-- numeros.

-- a) productoInterno: calcula el producto interno entre dos tuplas de R × R.

problema productoInterno((x1,y1):(RxR),(x2,y2):(R:R)):R {
    requiere{True}
    asegura{res sera el resultado de x1*x2 + y1*y2}
}

productoInterno :: (Float, Float) -> (Float, Float) -> Float
productoInterno (a, b) (c, d) = a * c + b * d

-- b) esParMenor: dadas dos tuplas de R × R, decide si cada coordenada de la primera tupla es menor a la coordenada correspondiente de la segunda tupla.

problema esParMenor((a,b):(R,R), (c,d):(R,R)): Bool {
    requiere{True}
    asegura{(res = true) <-> ( (a>c) AND (b>d) )}
}

esParMenor :: (Float, Float) -> (Float, Float) -> Bool
esParMenor (a,b) (c,d)  | (a>c) && (b>d) == True = True
                        | otherwise = False

-- c) distancia: calcula la distancia euclıdea entre dos puntos de R2. |(x1-x2,y1-y2)|
distancia :: (Float, Float) ->  (Float, Float) ->  Float
distancia (a,b) (c,d) = ((a-c)**2 + (b-d)**2)**(0.5)

-- d) sumaTerna: dada una terna de enteros, calcula la suma de sus tres elementos.
sumaTerna :: (Integer,Integer,Integer) -> Integer
sumaTerna (a,b,c) = a+b+c

-- e) sumarSoloMultiplos: dada una terna de n´umeros enteros y un natural, calcula la suma de los elementos de la terna que son m´ultiplos del n´umero natural.
sumarSoloMultiplos :: (Integer, Integer, Integer) -> Integer -> Integer
sumarSoloMultiplos (a,b,c) d    | mod a d == 0 && mod b d == 0 && mod c d == 0 = a + b + c
                                | mod a d == 0 && mod b d == 0 = a + b
                                | mod a d == 0 && mod c d == 0 = a + c
                                | mod b d == 0 && mod c d == 0 = b + c
                                | mod a == 0 = a
                                | mod b == 0 = b
                                | mod c == 0 = c
                                | otherwise = 0


--  f) posPrimerPar: dada una terna de enteros, devuelve la posici´on del primer n´umero par si es que hay alguno, o devuelve 4 si son todos impares.
-- usas mod y sale

-- g) crearPar :: a-> b-> (a, b): a partir de dos componentes, crea un par con esos valores. Debe funcionar para elementos de cualquier tipo.
crearPar :: t1 -> t2 -> (t1,t2)
crearPar t1 t2 = (t1,t2)

-- h) invertir :: (a, b)-> (b, a): invierte los elementos del par pasado como par´ametro. Debe funcionar para elementos de cualquier tipo.
invertir :: (a, b) ->(b, a)
invertir (a,b) = (b,a)

-- i) Reescribir los ejercicios productoInterno, esParMenor y distancia usando el siguiente renombre de tipos: type Punto2D = (Float, Float)
type Punto2D = (Float,Float)

productoInterno :: Punto2D -> Punto2D -> Float
productoInterno (a, b) (c, d) = a * c + b * d

esParMenor :: Punto2D -> Punto2D-> Bool
esParMenor (a,b) (c,d)  | (a>c) && (b>d) == True = True
                        | otherwise = False

distancia :: Punto2D ->  Punto2D ->  Float
distancia (a,b) (c,d) = ((a-c)**2 + (b-d)**2)**(0.5)