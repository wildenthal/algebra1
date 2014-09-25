quitar :: [Integer] -> Integer -> [Integer]
quitar a b  | length a== 0 = []
			| b/=(head a)= [head a] ++ quitar (tail a) b
			| otherwise = quitar ( tail a) b

repetidos :: [Integer] -> Integer -> [Integer]
repetidos a b	| length a== 0 = []
			| b==(head a)= [head a] ++ repetidos (tail a) b
			| otherwise = repetidos ( tail a) b
			
			
ordenar :: [Integer]-> [Integer]

ordenar a 	| (length a) ==0 = []
			| otherwise = repetidos a (maximo a) ++ ordenar ((quitar a) (maximo a))

maximo :: [Integer]-> Integer
maximo a | length a ==1 = head a
		 | (head a) > maximo (tail a) = head a
		 | otherwise = maximo (tail a)
