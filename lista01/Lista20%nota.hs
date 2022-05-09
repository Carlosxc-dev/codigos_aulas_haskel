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
recursiva n y 
    |y == 0 = 1
    |y >= 1 && y<= 5 = n
    |y>5 = n^5

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
reais:: Real -> Real -> Real
reais a b 
    |a < b = -(a*b)
    |a == b = 0
    |a > b == (a*b)

--Parte Dois: Recursão e Definições Locais

--ex13
funcao1:: Int -> Int -> Int 
funcao1 x y 
    |x == 0 = 0
    |mod y (funcao1 x-1) == 0 = 




