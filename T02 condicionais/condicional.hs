
maior :: Float -> Float -> Float
maior a b = if a < b --igual so coloca antes do if 
    then a 
    else b

maior2 :: Float -> Float -> Float
maior2 a b -- nao coloca igual pois nao e uma expressao
    | a > b = a
    | a < b = b
    | otherwise = 10000

--casamento de padroes 
fat1 :: Int -> Int
fat1 0 = 1
fat1 1 = 1
fat1 n = n * fat1 (n-1) 

fat2 :: Int -> Int
fat2 n = if n==0
    then 1
    else n * fat2(n-1)

fat3 :: Int -> Int
fat3 n  | n == 0 = 1
        | n > 0 = n*fat3(n-1)
        

fibo :: Int -> Int --problemas com tempo de exec..  
fibo 0 = 0
fibo 1 = 1
fibo n = fibo(n-1) + fibo(n-2)

par :: Int -> Bool 
par n = if mod n 2 == 0
    then True
    else False

maiusculo :: Char -> String
maiusculo l = if (l >= 'A') && ( l <= 'Z')
    then "maiusculo"
    else "minusculo"

param :: Int -> Int -> Int -> Int -- char sempre em aspas simples
param a b c | a == 0 = b^2 + 3*c 
            | a == 1 = 2*c^2 - 3*c 
            | a == 2 = 3*c - b^2
            | otherwise = 0
    
divRec :: Int -> Int -> Int
divRec a b  
    | a == b = 0 
    | a < b = a
    | a > b = divRec (a-b) b

multRec :: Int -> Int -> Int
multRec x 0 = 0
multRec x 1 = x
multRec x y = x + multRec x (y-1)

mdc :: Int -> Int -> Int
mdc x y
    | x > y = mdc (x-y) y
    | x < y = mdc x y 
    | x == y = x










