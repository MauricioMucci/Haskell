--Questao 1
maxi :: Int -> Int -> Int
maxi a b
    | a > b = a
    | otherwise = b

--Questao 2
vendas :: Int -> Int
vendas 0 = 100
vendas 1 = 500
vendas 2 = 300
vendas 3 = 100
vendas 4 = 250
vendas 5 = 600
vendas _ = 0

maiorVenda :: Int -> Int
maiorVenda 0 = vendas 0
maiorVenda n = maxi (maiorVenda(n - 1)) (vendas n)

--Questao 3
maxVenda :: Int -> Int
maxVenda n = achaSemana (maiorVenda n) (n)

--Questao 4
zeroVendas :: Int -> Int
zeroVendas n
    | ((vendas n) == 0) = n
    | ((vendas n) /= 0) = zeroVendas(n - 1)
    | otherwise = -1

--Questao 5
achaSemana :: Int -> Int -> Int
achaSemana s n
    | ((vendas n) == s) = n
    | ((vendas n /= s)) = achaSemana (s) (n - 1)

--Questao 6
{-
    Apenas trocaria o valor de s por 0, pois agora so necissito buscar
    por um unico numero e nao um numero que depende da escolha do usuario
-}

--Questao 8
fat :: Int -> Int
fat 0 = 1
fat n = n * fat(n - 1)

--Questao 9
fatorial :: Int -> Int -> Int
fatorial x y
    | x > y = x * fatorial(x - 1) (y)
    | otherwise = x

produto :: Int -> Int -> Int
produto x y
    | x == y = x * y
    | x > y = fatorial x y
    | x < y = fatorial y x

--Questao 10
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib(n - 1) + fib(n - 2)