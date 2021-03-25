--Questao 1
somaTuplas :: ((Int,Int),(Int,Int)) -> Int
somaTuplas ((a,b),(c,d)) = a + b + c + d

--Questao 2
shift :: ((Int, Int), Int) -> (Int, (Int, Int))
shift ((a, b), c) = (a,(b,c))

--Questao 3
maxi :: Int -> Int -> Int -> Int
maxi a b c
    | (a >= b) && (a >= c) = a
    | (b >= a) && (b >= c) = b
    | (c >= a) && (c >= b) = c

mini :: Int -> Int -> Int -> Int
mini a b c
    | (a <= b) && (a <= c) = a
    | (b <= a) && (b <= c) = b
    | (c <= a) && (c <= b) = c


minEmax :: Int -> Int -> Int -> (Int, Int)
minEmax a b c = (maxi a b c, mini a b c)

--Questao 4
vendas :: Int -> Int
vendas 0 = 100
vendas 1 = 500
vendas 2 = 300
vendas 3 = 100
vendas 4 = 250
vendas 5 = 600
vendas _ = 0

zeroVenda :: Int -> (Int, Bool)
zeroVenda n
    | ((vendas n) == 0) = (n,True)
    | otherwise = (-1, False)

--Questao 5
livro1 :: Livro
livro1 = ("Harry Potter and the Sorcerer's Stone","J.K.Rowling","85-325-1101-5")

livro2 :: Livro
livro2 = ("The Lord of The Rings: The Fellowship of the Ring","J.R.R.Tolkien","85-336-1340-7")

livro3 :: Livro
livro3 = ("Percy Jackson & The Olympians","Rick Riordan","85-980-7839-5")

type Livro = (String,String,String)

titulo :: Livro -> String
titulo (a,b,c) = a

autor :: Livro -> String
autor (a,b,c) = b

isbn :: Livro -> String
isbn (a,b,c) = c