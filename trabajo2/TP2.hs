data Proposicion = P
                 | Q
                 | R
                 | No Proposicion
                 | Y Proposicion Proposicion
                 | O Proposicion Proposicion
                 | Imp Proposicion Proposicion

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
showP P = "P"
showP Q = "Q"
showP R = "R"
showP (No p) = "~"++(showP p)
showP (Y p q) = "(" ++ (showP p) ++ " ^ " ++ (showP q) ++ ")"
showP (O p q) = "(" ++ (showP p) ++ " v " ++ (showP q) ++ ")"
showP (Imp p q) = "(" ++ (showP p) ++ " => " ++ (showP q) ++ ")"

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
