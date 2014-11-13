data Proposicion = P
                 | Q
                 | R
                 | No Proposicion
                 | Y Proposicion Proposicion
                 | O Proposicion Proposicion
                 | Imp Proposicion Proposicion
                 instance Show Proposicion where
		show P = "P"
		show Q = "Q"
		show R = "R"
		show (No p) = "~"++(show p)
		show (Y p q) = "(" ++ (show p) ++ " ^ " ++ (show q) ++ ")"
		show (O p q) = "(" ++ (show p) ++ " v " ++ (show q) ++ ")"
		show (Imp p q) = "(" ++ (show p) ++ " => " ++ (show q) ++ ")"

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
--ejercicio 3
eliminarImplicaciones :: Proposicion -> Proposicion
eliminarImplicaciones (Imp a b) = O (No(eliminarImplicaciones a)) (eliminarImplicaciones b)
eliminarImplicaciones a = a
