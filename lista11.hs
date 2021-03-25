-- Questao 1
data Dia = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo
    deriving(Eq,Show)

-- Questao 2
finalDeSemana :: Dia -> Bool
finalDeSemana Sabado = True
finalDeSemana Domingo = True
finalDeSemana _ = False

-- Questao 3
data TalvezFloat = Valor Float | Erro String
    deriving(Eq,Show);

-- Questao 4
divisao :: Float -> Float -> TalvezFloat
divisao _ 0 = Erro "Divisao Por Zero E Invalida"
divisao n1 n2 = Valor (n1/n2)

-- Questao 5
data Nat = Zero | Suc Nat
    deriving(Eq,Show)

dois :: Nat
dois = Suc (Suc Zero)

sete :: Nat
sete = Suc (Suc (Suc (Suc (Suc (Suc (Suc Zero))))))

natToint :: Nat -> Int
natToint Zero    = 0
natToint (Suc n) = 1 + natToint n

-- Questao 6
intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = Suc (intToNat (n - 1))