--Questao 1
concatena :: [[a]] -> [a]
concatena lista = foldr (++) [] lista

--Questao 2
andLista :: [Bool] -> Bool
andLista lista = foldr (&&) True lista

--Questao 3
somaQuadPos :: [Int] -> Int
somaQuadPos lista = foldr (+) 0 (map (^2) (filter (>0) lista))

--Questao 4
somaListas :: [[Int]] -> Int
somaListas lista = foldr (+) 0 (concatena lista)-- não consigo imaginar como usar a função map

--Questao 5
tamanhoListas :: [[a]] -> Int
tamanhoListas lista = length (foldr (++) [] lista)-- não consigo imaginar como usar a função map

--Questao 6
inverte :: [a] -> [a]
inverte []     = []
inverte (x:xs) = inverte xs ++ [x]-- não consigo imaginar como usar a função foldr

--Questao 7
separaPalavras :: String -> [String]
separaPalavras []      = []
separaPalavras palavra = (takeWhile (/= ' ') palavra) : (separaPalavras (drop 1 (dropWhile (/= ' ') palavra)))