type Texto = String
type Nombre = Texto
type Cantidad = Int
type Precio = Int
type ListaDePrecios = [(Nombre,Precio)]
type Mercaderia = [Nombre]


cantidadPorProducto :: Nombre -> Mercaderia -> Cantidad
cantidadPorProducto x [] = 0
cantidadPorProducto x (y:ys)    | x == y = 1 + cantidadPorProducto x ys
                                | otherwise = cantidadPorProducto x ys

quitarDeLaLista :: Nombre -> Mercaderia -> Mercaderia
quitarDeLaLista x [] = []
quitarDeLaLista x (y:ys)    | x == y = quitarDeLaLista x ys
                            | otherwise = [y] ++ quitarDeLaLista x ys

generarStock :: Mercaderia -> [(Nombre, Cantidad)]
generarStock [] = []
generarStock (m:ms) = [(m , cantidadPorProducto m (m:ms))] ++ generarStock (quitarDeLaLista m (m:ms))

listaMercaderia = ["sarten","espatula","espatula","sarten","espatula","sarten","sarten","cuchara","sarten","espatula","espatula","sarten","espatula","sarten","sarten","cuchara","sarten","espatula","espatula","sarten","espatula","sarten","sarten","cuchara","espatula","espatula"]
stock = generarStock listaMercaderia
listaPrecios = [("sarten",4.99),("espatula",9.99),("cuchara",2.0)]

stockDeProducto :: [(String, Int)]-> String -> Int
stockDeProducto [] producto = 0
stockDeProducto ((nombreProd,cantid):xs) producto | nombreProd == producto = cantid
                                                | otherwise = stockDeProducto xs producto

dineroEnStock :: [(String, Int)] -> [(String, Float)]->Float
dineroEnStock [] y = 0
dineroEnStock x [] = 0
dineroEnStock ((nombreProd,cant):xs) ((nombreP,precio):ys) = fromIntegral (stockDeProducto ((nombreProd,cant):xs) nombreProd) * precio + dineroEnStock xs ys

aplicarOferta :: [(String, Int)]->[(String, Float)]->[(String,Float)]
aplicarOferta [] y = []
aplicarOferta x [] = []
aplicarOferta ((nombreProd,cant):xs) ((nombreP,precio):ys)  | stockDeProducto ((nombreProd,cant):xs) nombreP > 10 = [(nombreP,precio*0.8)] ++ aplicarOferta xs ys
                                                            | otherwise = [(nombreP,precio)] ++ aplicarOferta xs ys


main :: IO()
main = do
    print(stock)
    print(listaPrecios)
    print(aplicarOferta stock listaPrecios)
