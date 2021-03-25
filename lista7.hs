--Questao 1
somaQuadrupla :: [(Int,Int,Int,Int)] -> Int
somaQuadrupla [] = 0
somaQuadrupla ((a,b,c,d):xs) = a + b + c + d + somaQuadrupla xs

--Questao 2
somaTuplas :: [((Int,Int),(Int,Int))] -> Int
somaTuplas [] = 0
somaTuplas (((a,b),(c,d)):xs) = a + b + c + d + somaTuplas xs

--Questao 3
zipp :: [Int] -> [Int] -> [(Int,Int)]
zipp (x:xs) (y:ys) = (x,y):zipp xs ys
zipp _ _ = []

--Questao 4
zipTres :: [a] -> [a] -> [a] -> [(a,a,a)]
zipTres (x:xs) (y:ys) (z:zs) = (x,y,z):zipTres xs ys zs
zipTres _ _ _ = []

--Questao 5
unZipp :: [(Int,Int)] -> ([Int], [Int])
unZipp ((a,b):xs) = (unzipEsq ((a,b):xs),unzipDir ((a,b):xs))
    where
        unzipEsq :: [(Int,Int)] -> [Int]
        unzipEsq [] = []
        unzipEsq ((a,b):xs) = a:unzipEsq xs
        unzipDir :: [(Int,Int)] -> [Int]
        unzipDir [] = []
        unzipDir ((a,b):xs) = b:unzipDir xs