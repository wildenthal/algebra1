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
showP a = showAuxiliar a

showAuxiliar :: Proposicion -> String
showAuxiliar (No a) = "~"++ (showAuxiliar a)
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

--Ejercicio 4
aFNN :: Proposicion -> Proposicion
aFNN p = aFNN2 (eliminarImplicaciones p)
 
aFNN2 :: Proposicion -> Proposicion
aFNN2 (No (Y a b)) = (O (aFNN2 (No a)) (aFNN2 (No b)))
aFNN2 (No (O a b)) = (Y (aFNN2 (No (eliminarImplicaciones a))) (aFNN2 (No (eliminarImplicaciones b))))
aFNN2 (No (No a)) = aFNN2 a
aFNN2 (No a) = No (aFNN2 a)
aFNN2 b = b


--Ejercicio 5
evaluar :: Proposicion -> (Bool, Bool, Bool) -> Bool
evaluar p (x, y, z) | p == P = x
                    | p == Q = y
                    | p == R = z
evaluar (Imp a b) terna = evaluar (eliminarImplicaciones (Imp a b)) terna
evaluar (Y a b) terna = (evaluar a terna) && (evaluar b terna)
evaluar (O a b) terna = (evaluar a terna) || (evaluar b terna)
evaluar (No a) terna = not (evaluar a terna)

--Ejercicio 6
combinacion :: Integer -> (Bool, Bool, Bool)
combinacion a = ( odd (div a 4), odd (div a 2), odd a)

--Ejercicio 7
data TipoFormula = Tautologia | Contradiccion | Contingencia deriving (Show)

tablaDeVerdadInfinita :: Proposicion -> Integer -> [Bool]
tablaDeVerdadInfinita p n = (evaluar p (combinacion n)) : (tablaDeVerdadInfinita p (n+1))

tipoDeFormula :: Proposicion -> TipoFormula
tipoDeFormula p | take 8 (tablaDeVerdadInfinita p 0) == (replicate 8 True) = Tautologia
                | take 8 (tablaDeVerdadInfinita p 0) == (replicate 8 False) = Contradiccion
                | otherwise = Contingencia
