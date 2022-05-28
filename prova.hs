-- carlos henrique cruz xavier 
-- matricula: 2021015751


--questao01 
average :: [Float] -> Float
average l = (soma l) / (quant l)

soma :: [Float] -> Float
soma [] = 0
soma (h:t) = h + soma t

quant :: [Float] -> Float
quant [] = 0
quant (h:t) = 1 + quant t 

--quetao 02
corrente :: Float -> Float -> Float
corrente v r = v / r

--questao 03
vogais :: String -> String
vogais [] = emply 
    where
        emply = []
vogais (h:t) 
    | h == 'a' = 'A' : vogais t
    | h == 'e' = 'E' : vogais t
    | h == 'i' = 'I' : vogais t
    | h == 'o' = 'O' : vogais t
    | h == 'u' = 'U' : vogais t
    | otherwise = h : vogais t

-- questao 04 
intersecao :: [Int] -> [Int] -> [Int]
intersecao l1 l2 = repetidos (l1 ++ l2)

repetidos :: [Int] -> [Int]
repetidos [] = []
repetidos (h:t) = h : repetidos (iguais h t)

iguais :: Int -> [Int] -> [Int]
iguais _ [] = []
iguais x (h:t) 
    | (x == h) = h : iguais x t
    | otherwise = iguais x t

--questao 05
separa :: [Int] -> [Int]
separa [] = []
separa (h:t) = if [x | x <- [0..h], mod h 2 == 0] == [1,h]
    then h : separa t
    else separa t
