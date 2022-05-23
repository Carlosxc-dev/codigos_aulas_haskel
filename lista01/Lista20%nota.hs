--Parte Um: Funções e Condicionais
--ex01
perimetroQuad:: Float -> Float -> Float
perimetroQuad w h = w*2 + h*2

areaTri:: Float -> Float -> Float
areaTri b h = (b*h) / 2

--ex02
velMedia:: Float -> Float -> Float
velMedia s t = s / t
difCarros:: Float -> Float -> Float -> Float
difCarros s t1 t2 = (velMedia s t1) - (velMedia s t2)

--ex03 
convertDolar::Float -> Float -> Float
convertDolar vl tx = vl / tx 

--ex04
temperatura:: Float -> Float 
temperatura c = c * 1.8 + 32

--ex05
isPar:: Int -> Bool
isPar x = if mod x 2 == 0
    then True
    else False

multiplo:: Int -> Int -> Bool 
multiplo i j = if mod i j == 0
    then True
    else False

--ex06
absoluto:: Char -> Int -> Int  -- nao consegui fazer
absoluto c x 
    |c == '-' = x
    |c == '+' = x

--ex07
menor:: Char -> Char -> Char
menor c1 c2 
    |c1 < c2 = c1
    |c2 < c1 = c2
    |otherwise = 'N'

--ex08 
menorGuard:: Float -> Float -> Float -> Float
menorGuard a b c 
    |a < b && a < c = a
    |b < c && b < c = b
    |otherwise = c

--ex09 
recursiva:: Int -> Int -> Int
recursiva n 0 = 1
recursiva n y = if (y>=1 && y<=5)
    then n
    else n^5


--ex10
ackerman:: Int -> Int -> Int -- terminar
ackerman m n 
    |m == 0 = n + 1
    |m > 0 && n == 0 = ackerman (m-1) 1
    |m > 0 && n > 0 = ackerman (m-1) (ackerman m (n-1))

--ex11
arestas:: Int -> Int -> Int -> Int 
arestas a b c 
    |a == b && a == c && c == b = 3
    |(a == b) || (a == c) || (b == c) = 2
    |otherwise = 1

--ex12
--fazer 12



--Parte Dois: Recursão e Definições Locais
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

primos :: Int -> Bool
primos x 
    |x < 1 = False 
    |x == 1 = True
    |otherwise = if (verif [2..x] x) == 1
        then True
        else False 
    
--ex 15
fibo :: Int -> Int 
fibo 0 = 0
fibo 1 = 1
fibo n = fibo(n-1) + fibo(n-2)

--ex 16
velocidade:: Float -> Float -> Float -> Float
velocidade vi vf t = vi*t + ((a*t^2)/2)
    where 
        a = (vf-vi)/t

-- ex17
somanatu:: Int-> Int -> Int
somanatu 0 y = y
somanatu x 0 = x
somanatu x y = if x /= 0 && y /= 0 && x > y 
    then somanatu (x+1) (y-1)
    else somanatu (x-1) (y+1)

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

--ex19
enesimo :: Int -> [Int] -> Int
enesimo n [] = 0  -- retornar contando com indice 0
enesimo n (t:h)
    | n == 0 = t
    | n /= 0 = enesimo (n-1) h

--ex20
addenesimo :: Int -> Int -> [Int] -> [Int]
addenesimo e 0 t = [e] ++ t
addenesimo e p (h:t) = [h] ++ addenesimo e (p-1) t

--ex21
subs:: Char -> Char -> String -> String
subs v n [] = []
subs v n (h:t) = if h == v
    then n : [] ++ subs v n t
    else h : [] ++ subs v n t

--ex22
inverte:: String -> String 
inverte [] = []
inverte (h:t) = inverte t ++ [h]

palin:: String -> Bool
palin str = str == (inverte str)

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












--[1,2,3,4,5] !! 3 -> retorna 4 ,3 e o indice da lista

