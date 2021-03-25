--Questao 1
osQuatroSaoIguais :: Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais a b c d =  (a == b) && (b == c) && (c == d)

--Questao 2
{-
    quantosSaoIguais :: Int -> Int -> Int -> Int
    quantosSaoIguais a b c
        | (a == b) && (b == c) = 3
        | (((a == b) && (b /= c)) || ((b == c) && (a /= c)) || ((a == c) && (a /= b))) = 2
        | otherwise = 0
-}

--Questao 3
todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes a b c = (a /= b) && (b /= c) && (a /= c)

--Questao 4
{-
    todosDiferentes n m p = ( ( n/=m ) && ( m/=p ) )

    Essa funcao esta errada pois "n" nao e verificado com "p". O certo seria o q foi feito no ex3

    todosDiferentes n m p = (n /= m) && (m /= p) && (n /= p)
-}

--Questao 5
todosIguais :: Int -> Int -> Int -> Bool
todosIguais a b c = (a == b) && (b == c)

quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais a b c
    | todosIguais a b c = 3
    | todosDiferentes a b c = 0
    | otherwise = 2

--Questao 6
elevadoDois :: Int -> Int
elevadoDois n = n * n

--Questao 7
elevadoQuatro :: Int -> Int
elevadoQuatro m = (elevadoDois m) * (elevadoDois m)

--Questao 8/1
vendas :: Int -> Int
vendas 0 = 0
vendas _ = 200

vendaTotal :: Int -> Int
vendaTotal 0 = vendas 0
vendaTotal n = vendas n + vendaTotal(n - 1)

--Questao 8/2
{-
    vendas :: Int -> Int
    vendas n
        | n == 0 = 0
        | otherwise = 200

    vendaTotal :: Int -> Int
    vendaTotal 0 = vendas 0
    vendaTotal n = vendas n + vendaTotal(n - 1)
-}
