--primeira lista de exercicios selecionados 
-- carlos henrique cruz xavier 
-- matricula 2021015751

--ex04
temperatura:: Float -> Float 
temperatura c = c * 1.8 + 32

--ex06
absoluto:: Char -> Int -> Int  -- nao consegui fazer
absoluto c x 
    |c == '-' = x
    |c == '+' = x

--ex11
arestas:: Int -> Int -> Int -> Int 
arestas a b c 
    |a == b && a == c && c == b = 3
    |(a == b) || (a == c) || (b == c) = 2
    |otherwise = 1

--ex13
funcao1:: [Int] -> Int -> Int
funcao1 [] _ = 0
funcao1 (h:t) y = if mod h y == 0
    then h + funcao1 t y
    else funcao1 t y

--ex14
verif :: [Int] -> Int -> Int
verif [] x = 0
verif (h:t) x = if mod x h == 0
    then 1 + verif t x
    else verif t x

--ex 16
velocidade:: Float -> Float -> Float -> Float
velocidade vi vf t = vi*t + ((a*t^2)/2)
    where 
        a = (vf-vi)/t

--ex18
uniao :: [Int] -> [Int] -> [Int]
uniao conj1 conj2 = retiraRepetidos (conj1 ++ conj2)

retiraRepetidos :: [Int] -> [Int]
retiraRepetidos [] = []
retiraRepetidos (a:cauda1) = a : retiraRepetidos (retiraTodos a cauda1)

retiraTodos :: Int -> [Int] -> [Int]
retiraTodos _ [] = []
retiraTodos x (a:cauda1) 
    | (x == a) = retiraTodos x cauda1
    | otherwise = a : retiraTodos x cauda1

--ex20
addenesimo :: Int -> Int -> [Int] -> [Int]
addenesimo e 0 t = [e] ++ t
addenesimo e p (h:t) = [h] ++ addenesimo e (p-1) t

--ex23
inicial:: [Int] -> Int -> [Int]
inicial lista 0 = []
inicial (h:t) n = h : inicial t (n-1)

final:: [Int] -> Int -> [Int]
final l n = inicial (inverso l) n

inverso:: [Int] -> [Int]
inverso [] = []
inverso (h:t) = (inverso t) ++ [h]

--ex26
positivos:: [Int] -> [Int]
positivos [] = []
positivos (h:t) 
    | h > 0 = h : positivos t
    | otherwise = positivos t




