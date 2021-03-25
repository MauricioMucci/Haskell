--Questao 1
head2 :: [a] -> a
head2 (x:_) = x
head2 []    = error "Lista Vazia"

tail2 :: [a] -> [a]
tail2 (_:xs) = xs
tail2 []     = error "Lista Vazia"

fst2 :: (a,b) -> a
fst2 (a,_) = a

shift :: ((x, y), z) -> (x, (y, z))
shift ((a, b), c) = (a,(b,c))

--Questao 2
concatena :: [[a]] -> [a]
concatena []     = []
concatena (x:xs) = x ++ concatena xs

--Questao 3
inverteLista :: [a] -> [a]
inverteLista []     = []
inverteLista (x:xs) = inverteLista xs ++ [x]

--Questao 4
zipTres :: [a] -> [b] -> [c] -> [(a,b,c)]
zipTres (x:xs) (y:ys) (z:zs) = (x,y,z):zipTres xs ys zs
zipTres _ _ _                = []

--Questao 5
mapMaisUm :: (Int -> a) -> [Int] -> [a]
mapMaisUm f []     = []
mapMaisUm f (x:xs) = f (x + 1) : mapMaisUm f xs

{-Questao 6
    foldr :: (a -> b -> b) -> b -> [a] -> b,
    possui esse tipo para generalizar o uso dela
-}