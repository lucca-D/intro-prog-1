{-- Sistema de stock
 Una reconocida empresa de comercio electr´onico nos pide desarrollar un sistema de stock de mercader´ıa. La mercader´ıa de
 la empresa va a ser representada como una secuencia de nombres de los productos, donde puede haber productos repetidos.
 El stock va a ser representado como una secuencia de tuplas de dos elementos, donde el primero es el nombre del producto y
 el segundo es la cantidad que hay en stock (en este caso no hay nombre de productos repetidos). Tambi´en se cuenta con una
 lista de precios de productos representada como una secuencia de tuplas de dos elementos, donde el primero es el nombre
 del producto y el segundo es el precio.
 Para implementar este sistema nos enviaron las siguientes especificaciones y nos pidieron que hagamos el desarrollo
 enteramente en Haskell, utilizando los tipos requeridos y solamente las funciones que se ven en la materia Introducci´on a la
 Programaci´on / Algoritmos y Estructuras de Datos I (FCEyN-UBA).--}

{--Ejercicio 1. Implementar la funci´on generarStock :: [String]->[(String, Int)]
 problema generarStock (mercader´ıa: seq⟨String⟩) : seq⟨String × Z⟩ {
 requiere: {True}
 asegura: { La longitud de res es igual a la cantidad de productos distintos que hay en mercader´ıa}
 asegura: {Para cada producto que pertenece a mercader´ıa, existe un i tal que 0 ≤ i < |res| y res[i]0=producto y
 res[i]1 es igual a la cantidad de veces que aparece producto en mercader´ıa}
 }--}

type Texto = String
type Nombre = Texto
type Cantidad = Int
type ListaDePrecios = [(Nombre,Precio)]
type Mercaderia = [Nombre]

quitarDeLaLista :: Nombre -> Mercaderia -> Mercaderia
quitarDeLaLista x [] = []
quitarDeLaLista x (y:ys)    | x == y = quitarDeLaLista x ys
                            | otherwise = [y] ++ quitarDeLaLista x ys

cantidadPorProducto :: Nombre -> Mercaderia -> Cantidad
cantidadPorProducto x [] = 0
cantidadPorProducto x (y:ys)    | x == y = 1 + cantidadPorProducto x ys
                                | otherwise = cantidadPorProducto x ys

generarStock :: Mercaderia -> [(Nombre, Cantidad)]
generarStock [] = []
generarStock (m:ms) = [(m , cantidadPorProducto m (m:ms))] ++ generarStock (quitarDeLaLista m (m:ms))

{-- Ejercicio 2. Implementar la funci´on stockDeProducto :: [(String, Int))]->String->Int
 problema stockDeProducto (stock: seq⟨String × Z⟩, producto: String ) : Z {
 requiere: {No existen dos nombres de productos (primeras componentes) iguales en stock}
 requiere: {Todas las cantidades (segundas componentes) de stock son mayores a cero}
 asegura: {si no existe un i tal que 0 ≤ i < |stock| y producto = stock[i]0 entonces res es igual a 0 }
 asegura: {si existe un i tal que 0 ≤ i < |stock| y producto = stock[i]0 entonces res es igual a stock[i]1 }
 }--}

-- supoongo la lista de mercaderia: ["sarten","espatula","espatula","sarten","espatula","sarten","sarten","cuchara"]
listaMercaderia = ["sarten","espatula","espatula","sarten","espatula","sarten","sarten","cuchara"]
stock = generarStock listaMercaderia

stockDeProducto :: [(String, Int)]-> String -> Int
stockDeProducto [] producto = 0
stockDeProducto ((nombreProd,cantid):xs) producto   | nombreProd == producto = cantid
                                                    | otherwise = stockDeProducto xs producto
{--Ejercicio 3. Implementar la funci´on dineroEnStock :: [(String, Int))]->[(String, Float)]->Float
problema dineroEnStock (stock: seq⟨String × Z⟩, precios: seq⟨String × R⟩ ) : R {
requiere: {No existen dos nombres de productos (primeras componentes) iguales en stock}
requiere: {No existen dos nombres de productos (primeras componentes) iguales en precios}
requiere: {Todas las cantidades (segundas componentes) de stock son mayores a cero}
requiere: {Todos los precios (segundas componentes) de precios son mayores a cero}
requiere: {Todo producto de stock aparece en la lista de precios}
asegura: {res es igual a la suma de los precios de todos los productos que est´an en stock multiplicado por la cantidad
de cada producto que hay en stock}
}
Para resolver este ejercicio pueden utilizar la funci´on del Preludio de Haskell fromIntegral que dado un valor de tipo
Int devuelve su equivalente de tipo Float.--}

dineroEnStock :: [(String, Int)] -> [(String, Float)]->Float
dineroEnStock [] y = 0
dineroEnStock x [] = 0
dineroEnStock ((nombreProd,cant):xs) ((nombreP,precio):ys)  | fromIntegral (stockDeProducto ((nombreProd,cant):xs) nombreProd) * precio + dineroEnStock xs ys

{-- Ejercicio 4. Implementar la funci´on aplicarOferta :: [(String, Int)]->[(String, Float)]->[(String,Float)]
 problema aplicarOferta (stock: seq⟨String × Z⟩, precios: seq⟨String × R⟩ ) : seq⟨String × R⟩ {
 requiere: {No existen dos nombres de productos (primeras componentes) iguales en stock}
 requiere: {No existen dos nombres de productos (primeras componentes) iguales en precios}
 requiere: {Todas las cantidades (segundas componentes) de stock son mayores a cero}
 requiere: {Todos los precios (segundas componentes) de precios son mayores a cero}
 requiere: {Todo producto de stock aparece en la lista de precios}
 asegura: {|res| = |precios|}
 asegura: {Para todo 0 ≤ i < |precios|, si stockDeProducto(stock, precios[i]0) > 10, entonces res[i]0 = precios[i]0 y
 res[i]1 = precios[i]1∗ 0,80}
 asegura: {Para todo 0 ≤ i < |precios|, si stockDeProducto(stock, precios[i]0) ≤ 10, entonces res[i]0 = precios[i]0 y
 res[i]1 = precios[i]1 }
 }--}

aplicarOferta :: [(String, Int)]->[(String, Float)]->[(String,Float)]
aplicarOferta [] y = []
aplicarOferta x [] = []
aplicarOferta ((nombreProd,cant):xs) ((nombreP,precio):ys)  | stockDeProducto ((nombreProd,cant):xs) nombreP > 10 = [(nombreP,precio*08.80)] ++ aplicarOferta xs ys
                                                            | otherwise = [(nombreP,precio)] ++ aplicarOferta xs ys