--Questao 1
pegaPosicao :: Int -> [Int] -> Int
pegaPosicao m n
    | m > length n = error "Posicao nao existente" 
    | otherwise = head(drop m n)

--Questao 2
pega :: Int -> [Int] -> [Int]
pega a b = take a b

--Questao 3
retira :: Int -> [Int] -> [Int]
retira a b = drop a b

--Questao 4
mediaLista :: [Float] -> Float
mediaLista [] = 0
mediaLista a = somaLista a / tamanho a
    where
        tamanho :: [Float] -> Float
        tamanho [] = 0
        tamanho (x:xs) = 1 + tamanho xs
        somaLista :: [Float] -> Float
        somaLista [] = 0
        somaLista (x:xs) = x + somaLista xs

--Questao 5
pegaMaiores :: Int -> [Int] -> [Int]
pegaMaiores a [] = []
pegaMaiores a b
    | a < head b = head b : pegaMaiores a (tail b) 
    | otherwise = pegaMaiores a (tail b)

--Questao 6
contaMaiores :: Int -> [Int] -> Int
contaMaiores a [] = 0
contaMaiores a b
    | a < head b = 1 + contaMaiores a (tail b)
    | otherwise = contaMaiores a (tail b)

--Questao 7
intercala :: [Int] -> [Int] -> [Int]
intercala [] [] = []
intercala (x:xs) [] = (x:xs)
intercala [] (z:zs) = (z:zs)
intercala (x:xs) (z:zs) = x:z:intercala xs zs

--Questao 8
dupli :: [Int] -> [Int]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

--Questao 9

--Questao 10
lista :: [Char]
lista = ['a','b','c','d','e','f','g','h','i','k']

dropEvery :: [Char] -> Int -> [Char]
dropEvery xs n
    | length xs < n = xs
    | otherwise = take (n - 1) xs ++ dropEvery (drop n xs) n
    
--Questao 11
