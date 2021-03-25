--Funcoes Extras
dobra :: Int -> Int
dobra x = x * x

incrementa :: Int -> Int
incrementa x = x + 1

vendas :: Int -> Int
vendas 0 = 100
vendas 1 = 500
vendas 2 = 300
vendas 3 = 100
vendas 4 = 250
vendas 5 = 600
vendas _ = 0

soma :: Int -> Int -> Int
soma x y = x + y

mult :: Int -> Int -> Int
mult x y = x * y

naoEspaco :: Char -> Bool
naoEspaco x = x /= ' '

mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt f [] = []
mapInt f (x:xs) = f x : mapInt f xs

--Questao 1
aplicaDuasVezes :: (Int -> Int) -> Int -> Int
aplicaDuasVezes f a = f (f a)

--Questao 2
vendaTotal :: (Int -> Int) -> Int -> Int
vendaTotal f 0 = f 0
vendaTotal f x = (f x) + vendaTotal f (x - 1)

--Questao 3
foldInt :: (Int -> Int -> Int) -> [Int] -> Int
foldInt f [] = error "Lista Vazia"
foldInt f (x:[]) = x
foldInt f (x:xs) = f x (foldInt f xs)

--Questao 4
filterString :: (Char -> Bool) -> [Char] -> [Char]
filterString f [] = []
filterString f (x:xs)
    | f x = x:filterString f xs
    | otherwise = filterString f xs

--Questao 5
somaQuadrado :: [Int] -> Int
somaQuadrado [] = 0
somaQuadrado (x:xs) = foldInt (+) (mapInt (^2) (x:xs))

--Questao 6
iter :: (a -> a) -> a -> [a]
iter f x = x : iter f (f x) 