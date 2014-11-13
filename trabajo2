data Proposicion = P
                 | Q
                 | R
                 | No Proposicion
                 | Y Proposicion Proposicion
                 | O Proposicion Proposicion
                 | Imp Proposicion Proposicion

--Ejercicio 1
atomoONegacion :: Proposicion -> Bool
atomoONegacion No _ = True
atomoONegacion x | x == P = True
                 | x == Q = True
                 | x == R = True
                 | otherwise = False

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
