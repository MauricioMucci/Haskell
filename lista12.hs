data Arvore a = Folha a | Nodo a (Arvore a) (Arvore a)
    deriving(Eq,Show)

arv1 :: Arvore Int
arv1 = Nodo 10 (Nodo 14 (Nodo 1 (Nodo 4 (Folha 9) (Nodo 14 (Folha 2) (Folha 1))) (Folha 2)) (Folha 6)) (Nodo 9 (Folha 10) (Folha 6))

-- Questao 1
multDois :: Arvore Int -> Arvore Int
multDois (Folha n)      = Folha (n*2)
multDois (Nodo n a1 a2) = Nodo (n * 2) (multDois a1) (multDois a2)

-- Questao 2
contaElementos :: Arvore a -> Int
contaElementos (Folha n)    = 1
contaElementos (Nodo n e d) = 1 + contaElementos e + contaElementos d

-- Questao 3
altura :: Arvore a -> Int
altura (Folha n) = 1   
altura (Nodo n e d)
    | contaElementos e > contaElementos d = 1 + altura e
    | otherwise                           = 1 + altura d

-- Questao 4
maiorElemento :: Arvore Int -> Int
maiorElemento x = head (drop (length (arvoreToLista x)-1) (quickSort (arvoreToLista x)))
    where
        quickSort :: [Int] -> [Int]     
        quickSort [] = []
        quickSort (x:xs) =  quickSort (menores x xs) ++ [x] ++ quickSort (maiores x xs)
        menores :: Int -> [Int] -> [Int]
        menores n [] = []
        menores n (x:xs) 
            | x <= n = x: menores n xs
            | otherwise = menores n xs                                                                                             
        maiores :: Int -> [Int] -> [Int]
        maiores n [] = []
        maiores n (x:xs) 
            | x > n = x : maiores n xs
            | otherwise = maiores n xs

-- Questao 5
procuraInt :: Int -> Arvore Int -> Bool
procuraInt x (Folha n)
    | x == n    = True
    | otherwise = False
procuraInt x (Nodo n e d)
    | n == x    = True
    | otherwise = False || (procuraInt x e) || (procuraInt x d)

-- Questao 6
quantasVezes :: Int -> Arvore Int -> Int
quantasVezes x (Folha n)
    | x == n    = 1 
    | otherwise = 0
quantasVezes x (Nodo n e d)
    | x == n    = 1 + quantasVezes x e + quantasVezes x d
    | otherwise = quantasVezes x e + quantasVezes x d

-- Questao 7
refleteArvore :: Arvore a -> Arvore a
refleteArvore (Folha n)    = Folha (n)
refleteArvore (Nodo n e d) = Nodo n (refleteArvore d) (refleteArvore e)

-- Questao 8
arvoreToLista :: Arvore a -> [a]
arvoreToLista (Folha n)    = [n]
arvoreToLista (Nodo n e d) = arvoreToLista e ++ [n] ++ arvoreToLista d

-- Questao 9
mapTree :: (a -> b) -> Arvore a -> Arvore b
mapTree f (Folha n)    = Folha (f n)
mapTree f (Nodo n e d) = Nodo (f n) (mapTree f e) (mapTree f d)