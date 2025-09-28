{--Ejercicio 6. En este ejercicio trabajaremos con la lista de contactos del tel´efono.--}

{-- a) Implementar una funci´on que me diga si una persona aparece en mi lista de contactos del tel´efono: enLosContactos ::
 Nombre-> ContactosTel-> Bool--}

type Texto = [Char]
type Nombre = Texto
type Telefono = Texto
type Contacto = (Nombre, Telefono)
type ContactosTel = [Contacto]

enLosContactos :: Nombre -> ContactosTel -> Bool
enLosContactos [] l = False
enLosContactos s [] = False
--enLosContactos s [(nombre,numero)]      | s == nombre = True
--                                        | otherwise = False
enLosContactos s ((nombre,numero):ls)   | s == nombre = True
                                        | otherwise = enLosContactos s ls

{--b) Implementar una funci´on que agregue una nueva persona a mis contactos, si esa persona est´a ya en mis contactos entonces
 actualiza el tel´efono. agregarContacto :: Contacto-> ContactosTel-> ContactosTel--}

actualizarContacto :: Contacto -> ContactosTel-> ContactosTel
actualizarContacto ([],_) l = l
actualizarContacto n [] = []
actualizarContacto (nombreC,numeroC) ((nombreL,numeroL):ls)    | nombreC == nombreL = [(nombreL,numeroC)] ++ actualizarContacto (nombreC,numeroC) ls
                                                                | otherwise = [(nombreL,numeroL)] ++ actualizarContacto (nombreC,numeroC) ls
agregarContacto :: Contacto-> ContactosTel-> ContactosTel
agregarContacto ([],_) l = l
agregarContacto c [] = [c]
agregarContacto (nombreC,numeroC) l     | enLosContactos nombreC l = actualizarContacto (nombreC,numeroC) l
                                        | otherwise = l ++ [(nombreC,numeroC)]

{--c) Implementar una funci´on que dado un nombre, elimine un contacto de mis contactos. Si esa persona no est´a no hace
 nada. eliminarContacto :: Nombre-> ContactosTel-> ContactosTel--}

eliminarContactoAux :: Contacto-> ContactosTel-> ContactosTel
eliminarContactoAux ([],_) l = l
eliminarContactoAux c [] = []
eliminarContactoAux (nombreC,numeroC) ((nombreL,numeroL):ls)    | nombreC == nombreL = [] ++ eliminarContactoAux (nombreC,numeroC) ls
                                                                | otherwise = [(nombreL,numeroL)] ++ eliminarContactoAux (nombreC,numeroC) ls

eliminarContacto :: Contacto-> ContactosTel-> ContactosTel
eliminarContacto ([],_) l = l
eliminarContacto c [] = [c]
eliminarContacto (nombreC,numeroC) l    | enLosContactos nombreC l = eliminarContactoAux (nombreC,numeroC) l
                                        | otherwise = l