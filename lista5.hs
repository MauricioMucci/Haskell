--Questao 1
{-
membro :: Int -> [Int]-> Bool
membro m [] = False
membro m (x:xs)
    | (m == x)  = True
    | otherwise = membro m xs
-}

--Questao 2
membroNum :: Int -> [Int] -> Int
membroNum m [] = 0
membroNum m (x:xs)
    | (m == x)  = 1 + membroNum m xs
    | otherwise = membroNum m xs

--Questao 3
membro :: Int -> [Int]-> Bool
membro m (x:xs)
    | (membroNum m (x:xs)) > 0 = True
    | otherwise = False

--Questao 4
unico :: [Int] -> [Int]
unico [] = []
unico (x:xs)
    | membroNum x xs == 0 = x : unico xs
    | otherwise = unico (removeDaLista x xs)
        where
            removeDaLista :: Int -> [Int] -> [Int]
            removeDaLista a [] = []
            removeDaLista a (x:xs)
                | a == x = removeDaLista a xs
                | otherwise = x:removeDaLista a xs

--Questao 5
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) = quickSort (menores x xs) ++ [x] ++ quickSort (maiores x xs)
    where
        menores :: Int -> [Int] -> [Int]
        menores x [] = []
        menores x (y:ys)
            | y < x     = y:menores x ys
            | otherwise = menores x ys
        maiores :: Int -> [Int] -> [Int]
        maiores x [] = []
        maiores x (y:ys)
            | y > x     = y:maiores x ys
            | otherwise = maiores x ys