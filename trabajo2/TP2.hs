data Proposicion = P
                 | Q
                 | R
                 | No Proposicion
                 | Y Proposicion Proposicion
                 | O Proposicion Proposicion
                 | Imp Proposicion Proposicion
 deriving (Eq)

instance Show Proposicion where
 show x = showP x

--Ejercicio 1
atomoONegacion :: Proposicion -> Bool
atomoONegacion P = True
atomoONegacion Q = True
atomoONegacion R = True
atomoONegacion (No _) = True
atomoONegacion _ = False

--Ejercicio 2
showP :: Proposicion -> String
showP (Y a b) = showAuxiliar a ++ " ^ " ++ showAuxiliar b
showP (O a b) = showAuxiliar a ++ " v " ++ showAuxiliar b
showP (Imp a b) = showAuxiliar a ++ " => " ++ showAuxiliar b
showP (No a) = "~" ++ showAuxiliar a

showAuxiliar :: Proposicion -> String
showAuxiliar a | a == P = "P"
               | a == Q = "Q"
               | a == R = "R"
               | otherwise = "(" ++ showP a ++ ")"

--Ejercicio3
eliminarImplicaciones :: Proposicion -> Proposicion
eliminarImplicaciones (Imp a b) = O (No (eliminarImplicaciones a)) (eliminarImplicaciones b)
eliminarImplicaciones (O a b) = O (eliminarImplicaciones a) (eliminarImplicaciones b)
eliminarImplicaciones (Y a b) = Y (eliminarImplicaciones a) (eliminarImplicaciones b)
eliminarImplicaciones (No a) = No (eliminarImplicaciones a)
eliminarImplicaciones a = a

--Ejercicio 6
combinacion :: Integer -> (Bool, Bool, Bool)
combinacion a = ( odd (div a 4), odd (div a 2), odd a)
