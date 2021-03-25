--Questao 1
lista1 :: [Int]
lista1 = [0,2 .. 10]

lista2 :: [Int]
lista2 = [1,3 .. 11]

multDoisLista :: [Int] -> [Int]
multDoisLista [] = []
multDoisLista (x:xs) = (2 * x) : multDoisLista xs 

--Questao 2
tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

--Questao 3
produtoLista :: [Int] -> Int
produtoLista [] = 1
produtoLista (x:xs) = x * produtoLista xs

--Questao 4
andLista :: [Bool] -> Bool
andLista [] = True
andLista (x:xs) = x && andLista xs

--Questao 5
concatLista :: [[Int]] -> [Int]
concatLista [] = []
concatLista (x:xs) = x ++ concatLista xs

--Questao 6
inverteLista :: [Int] -> [Int]
inverteLista [] = []
inverteLista (x:xs) = inverteLista xs ++ [x]