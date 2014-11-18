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
atomoONegacion (No _) = True
atomoONegacion P = True
atomoONegacion Q = True
atomoONegacion R = True
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

--Ejercicio 3
eliminarImplicaciones :: Proposicion -> Proposicion
eliminarImplicaciones (Imp a b) = O (No (eliminarImplicaciones a)) (eliminarImplicaciones b)
eliminarImplicaciones (O a b) = O (eliminarImplicaciones a) (eliminarImplicaciones b)
eliminarImplicaciones (Y a b) = Y (eliminarImplicaciones a) (eliminarImplicaciones b)
eliminarImplicaciones (No a) = No (eliminarImplicaciones a)
eliminarImplicaciones a = a

--Ejercicio 5
evaluar :: Proposicion -> (Bool, Bool, Bool) -> Bool
evaluar p (x, y, z) | p == P = x
                    | p == Q = y
                    | p == R = z
evaluar (Imp a b) terna | (evaluar b terna) == False && (evaluar a terna) == True = False
                        | otherwise = True
evaluar (Y a b) terna | (evaluar a terna) == True && (evaluar b terna) == True = True
                      | otherwise = False
evaluar (O a b) terna | (evaluar a terna) == False && (evaluar b terna) == False = False
                      | otherwise = True
evaluar (No a) terna = not (evaluar a terna)

--Ejercicio 6
combinacion :: Integer -> (Bool, Bool, Bool)
combinacion a = ( odd (div a 4), odd (div a 2), odd a)
