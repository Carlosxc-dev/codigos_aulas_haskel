type NomeAluno = String
type MediaNota = Int
type Aluno = (NomeAluno , MediaNota)
type Turma = [Aluno]

approved:: Turma -> MediaNota -> [NomeAluno]
approved tma notaCorte = 
    [nome | (nome, nota) <- tma , nota >= notaCorte]


type X = Int
type Y = Int
type Z = Int
type Ponto = (Float, Float, Float)

distancia:: Ponto -> Ponto -> Float 
distancia (x1, y1, z1) (x2, y2, z2)= sqrt (dx^2 + dy^2 + dz^2)
    where
        dx = x1 - x2
        dy = y1 - y2
        dz = z1 - z2

-- maior :: Int -> Int -> Bool
-- maior a b = a > b 

-- menor :: Int -> Int -> Bool
-- menor a b = a < b

-- filtro :: (Int -> Bool) -> [Int] -> [Int]
-- filtro funcao [] = [] 
-- filtro funcao (h:t)
--     |(funcao h) == True = h : (filtro funcao t)
--     |otherwise = filtro funcao t 


-- par :: Int -> Bool
-- par x = (mod x 2 == 0)

-- impar :: Int -> Bool
-- impar x = (mod x 2 == 1)

mapint :: (Int -> Int) -> [Int] -> [Int]
mapint func [] = []
mapint func (h:t) = (func h):(mapint func t)

dobra :: Int -> Int 
dobra x = x + x

quad :: Int -> Int 
quad x = x^2
